import { BlockHeader } from './../api/interfaces/op-energy.interface';
import Bluebird = require('bluebird');
import bitcoinApi from '../../api/bitcoin/bitcoin-api-factory';
import config from '../../config';
import logger from '../../logger';
import opBlockHeaderRepository from '../repositories/OpBlockHeaderRepository';

export class OpBlockHeaderService {
  async $getBlockHeaderData(blockHeight: number) {
    try {
      // ignoring first 6 blocks
      if (blockHeight <= config.OP_ENERGY.CONFIRMED_BLOCKS_AMOUNT) {
        return null;
      }

      // Only storing 6th block from current tip
      const blockHash = await bitcoinApi.$getBlockHash(
        blockHeight - config.OP_ENERGY.CONFIRMED_BLOCKS_AMOUNT
      );
      const {
        height,
        version,
        previousblockhash,
        merkle_root,
        timestamp,
        difficulty,
        nonce,
        chainwork,
        mediantime,
      } = await bitcoinApi.$getBlock(blockHash);

      const { totalfee, subsidy } = await bitcoinApi.$getBlockStats(blockHash);

      return {
        height,
        version,
        chainWork: chainwork,
        medianTime: mediantime,
        previousBlockHash: previousblockhash,
        merkleRoot: merkle_root,
        timestamp,
        difficulty,
        nonce,
        reward: totalfee + subsidy,
      };
    } catch (error) {
      logger.err(
        `Error while fetching block header ${blockHeight - 6}: ${error}`
      );
      throw error;
    }
  }

  async $saveBlockHeader(blockHeader: BlockHeader): Promise<void> {
    try {
      await opBlockHeaderRepository.$saveBlockHeaderInDatabase(blockHeader);
    } catch (error) {
      logger.err(
        `Error while saving block header ${blockHeader.height}: ${error}`
      );
      throw error;
    }
  }

  public async $syncOlderBlockHeader(currentTip?: number): Promise<void> {
    try {
      logger.debug('Syncing older block headers');

      // Adding confirmed block in synced block to get actual synced blocked height
      let currentSyncedBlockHeight =
        (await opBlockHeaderRepository.$getLatestBlockHeight()) +
        config.OP_ENERGY.CONFIRMED_BLOCKS_AMOUNT;

      let currentBlockHeight = currentTip;

      // Only using fetching current block tip if not present in argument
      if (!currentBlockHeight) {
        currentBlockHeight = await bitcoinApi.$getBlockHeightTip();
      }

      logger.debug(
        `currentSyncedBlockHeight: ${currentSyncedBlockHeight}, currentBlockHeaderHeight: ${currentBlockHeight}`
      );

      if (currentSyncedBlockHeight >= currentBlockHeight) {
        logger.debug('Already synced block headers.');
        return;
      }

      currentSyncedBlockHeight += 1;

      logger.debug(
        `Syncing block header from #${currentSyncedBlockHeight} to #${currentBlockHeight}`
      );

      while (currentBlockHeight >= currentSyncedBlockHeight) {
        const noOfBlockHeaders = Math.max(
          1,
          Math.min(10, currentBlockHeight - currentSyncedBlockHeight)
        );

        const blockHeaders = await Bluebird.map(
          [...Array(noOfBlockHeaders).keys()].map(
            (i) => i + currentSyncedBlockHeight
          ),
          (height) => this.$getBlockHeaderData(height)
        );

        await Bluebird.mapSeries(blockHeaders, (blockHeader) =>
          opBlockHeaderRepository.$saveBlockHeaderInDatabase(blockHeader as BlockHeader)
        );
        
        currentSyncedBlockHeight += noOfBlockHeaders;
      }

      logger.debug('Synced all the missing block headers');
    } catch (error) {
      logger.err(`Something went wrong while syncing block header.` + error);
    }
  }

  public async $getBlockHeader(height: number): Promise<BlockHeader | null> {
    try {
      const blockHeader = await opBlockHeaderRepository.$getBlock(height);
      return blockHeader;
    } catch (error) {
      logger.err(`Something went wrong while fetching block header.` + error);
      throw error;
    }
  }
}

export default new OpBlockHeaderService();

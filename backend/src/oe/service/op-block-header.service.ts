import { BlockHeader, BlockHeight, ConfirmedBlockHeight, BlockHash } from './../api/interfaces/op-energy.interface';
import Bluebird = require('bluebird');
import bitcoinApi from '../../api/bitcoin/bitcoin-api-factory';
import config from '../../config';
import logger from '../../logger';
import opBlockHeaderRepository from '../repositories/OpBlockHeaderRepository';

export class OpBlockHeaderService {
  async $getBlockHeaderData(blockHeight: ConfirmedBlockHeight): Promise<BlockHeader> {
    try {
      const DEFAULT_SUBSIDY = 5000000000;

      const blockHash = await bitcoinApi.$getBlockHash(
        blockHeight.value
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

      let fees: number = 0;
      let subsidy: number = DEFAULT_SUBSIDY;

      /* 
        Due to an issue in the blockchain's API, it does not return any stat value for the zeroth block, instead throws an error. To avoid the error we are first checking the block height and use default value for it 
      */
      if (blockHeight.value > 0) {
        const stats = await bitcoinApi.$getBlockStats(blockHash);
        fees = stats.totalfee;
        subsidy = stats.subsidy;
      }

      return {
        height,
        version,
        chainwork: chainwork,
        mediantime: mediantime,
        previous_block_hash: previousblockhash,
        merkle_root: merkle_root,
        timestamp,
        difficulty,
        nonce,
        reward: fees + subsidy,
        current_block_hash: blockHash
      };
    } catch (error) {
      logger.err(
        `Error while fetching block header ${blockHeight.value}: ${error}`
      );
      throw error;
    }
  }

  /**
   * returns BlockHeader of the last confirmed block
   */
  public async $syncOlderBlockHeader(UUID: string, currentTip?: number): Promise<BlockHeader> {
    let latestConfirmedBlockHeight : ConfirmedBlockHeight | undefined = undefined;
    try {
      logger.debug('Syncing older block headers');

      let currentSyncedBlockHeight = -1;
      try {
        let { value: latestStoredBlockHeight } = await opBlockHeaderRepository.$getLatestBlockHeight(UUID);
        currentSyncedBlockHeight = latestStoredBlockHeight; // there is some blocks had ben stored previously
      } catch(error) {
        logger.debug('there are no older block stored previously, starting from 0');
      }


      // Only using fetching current block tip if not present in argument
      if (!currentTip) {
        currentTip = await bitcoinApi.$getBlockHeightTip();
      }
      latestConfirmedBlockHeight = this.verifyConfirmedBlockHeight(currentTip - config.OP_ENERGY.CONFIRMED_BLOCKS_AMOUNT, { value: currentTip });


      logger.debug(
        `currentSyncedBlockHeight: ${currentSyncedBlockHeight}, latestConfirmedBlockHeight: ${latestConfirmedBlockHeight.value}`
      );

      if (currentSyncedBlockHeight >= latestConfirmedBlockHeight.value) {
        logger.debug('Already synced block headers.');
        return await this.$getBlockHeaderData( latestConfirmedBlockHeight);
      }

      currentSyncedBlockHeight += 1;

      logger.debug(
        `Syncing block header from #${currentSyncedBlockHeight} to #${latestConfirmedBlockHeight.value}`
      );

      while (latestConfirmedBlockHeight.value >= currentSyncedBlockHeight) {
        const noOfBlockHeaders = Math.max(
          1,
          Math.min(10, latestConfirmedBlockHeight.value - currentSyncedBlockHeight)
        );

        const blockHeaders = await Bluebird.map(
          [...Array(noOfBlockHeaders).keys()].map(
            (i) => i + currentSyncedBlockHeight
          ),
          (height) => this.$getBlockHeaderData({ value: height })
        );

        await Bluebird.mapSeries(blockHeaders, (blockHeader) =>
          opBlockHeaderRepository.$saveBlockHeaderInDatabase(UUID, blockHeader as BlockHeader)
        );

        currentSyncedBlockHeight += noOfBlockHeaders;
      }
      logger.debug('Synced all the missing block headers');
    } catch (error) {
      logger.err(`Something went wrong while syncing block header.` + error);
      throw new Error(`Something went wrong while syncing block header.` + error);
    }
    if( !latestConfirmedBlockHeight) {
      throw new Error( 'unable to get latestConfirmedBlockHeight');
    }
    return await this.$getBlockHeaderData( latestConfirmedBlockHeight);
  }

  public async $getBlockHeader(UUID: string, height: ConfirmedBlockHeight): Promise<BlockHeader> {
    try {
      const blockHeader = await opBlockHeaderRepository.$getBlock(UUID, height);
      return blockHeader;
    } catch (error) {
      logger.err(`Something went wrong while fetching block header.` + error);
      throw error;
    }
  }

  public async $getBlockHeadersByHeights(UUID: string, bockHeights: ConfirmedBlockHeight[]): Promise<BlockHeader[]> {
    try {
      const blockHeader = await opBlockHeaderRepository.$getBlockHeadersByHeights(UUID, bockHeights);
      return blockHeader;
    } catch (error) {
      logger.err(`Something went wrong while fetching block header range.` + error);
      throw error;
    }
  }

  /**
   * see OpBlockHeaderRepository.$getBlockHeaderByHash for reference as this function is just a wrapper over it
   */
  public async $getBlockHeaderByHash( UUID: string, blockHash: BlockHash): Promise<BlockHeader> {
    return await opBlockHeaderRepository.$getBlockHeaderByHash( UUID, blockHash);
  }

  public verifyConfirmedBlockHeight(blockHeight: number, currentTip: BlockHeight): ConfirmedBlockHeight {
    if (blockHeight > currentTip.value - config.OP_ENERGY.CONFIRMED_BLOCKS_AMOUNT) {
      throw new Error('block height haven\'t been confirmed');
    }
    return { value: blockHeight };
  }

  /**
   * Returns current tip for new block header table
   * @param UUID string 
   * @returns ConfirmedBlockHeight
   */
  public async getBlockHeightTip(UUID: string): Promise<ConfirmedBlockHeight> {
    return await opBlockHeaderRepository.$getLatestBlockHeight(UUID);
  }
}

export default new OpBlockHeaderService();

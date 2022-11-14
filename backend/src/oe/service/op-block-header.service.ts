import Bluebird = require('bluebird');
import bitcoinApi from '../../api/bitcoin/bitcoin-api-factory';
import config from '../../config';
import logger from '../../logger';
import opBlockHeaderRepository from '../repositories/OpBlockHeaderRepository';

export class OpBlockHeaderService {

  async $saveBlockHeader(blockHeight: number): Promise<void> {
    try {
      // ignoring first 6 blocks
      if (blockHeight <= 6) {
        return;
      }

      // Only storing 6th block from current tip
      const blockHash = await bitcoinApi.$getBlockHash(blockHeight - 6);
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

      await opBlockHeaderRepository.$saveBlockHeaderInDatabase({
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
      });
    } catch (error) {
      logger.err(
        `Error while saving block header ${blockHeight - 6}: ${error}`
      );
    }
  }

  public async $syncOlderBlockHeader(): Promise<void> {
    try {
      logger.debug('Syncing older block headers');

      let currentSyncedBlockHeight =
        await opBlockHeaderRepository.$getLatestBlockHeight();
      const currentBlockHeight =
        (await bitcoinApi.$getBlockHeightTip()) -
        config.MEMPOOL.INITIAL_BLOCKS_AMOUNT;

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
        await Bluebird.map(
          [...Array(10).keys()].map((i) => i + currentSyncedBlockHeight),
          (height) => this.$saveBlockHeader(height)
        );
        currentSyncedBlockHeight += 10;
      }

      logger.debug('Synced all the missing block headers');
    } catch (error) {
      logger.err(`Something went wrong while syncing block header.` + error);
    }
  }
}

export default new OpBlockHeaderService();

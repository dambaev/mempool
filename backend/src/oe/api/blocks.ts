import bitcoinApi from './../../api/bitcoin/bitcoin-api-factory';
import config from '../../config';
import logger from '../../logger';
import { Common } from './../../api/common';
import memPool from './../../api/mempool';
import transactionUtils from './../../api/transaction-utils';
import { IEsploraApi } from './../../api/bitcoin/esplora-api.interface';
import { BlockExtended, PoolTag, TransactionExtended, TransactionMinerInfo } from './../../mempool.interfaces';

class OEBlocks {

  public async getExtendedBlocktxIdsTransactionsByBlockHeight$( height: number): Promise<[BlockExtended, string[], TransactionExtended[] ] | undefined> {
    const blockHeightTip = await bitcoinApi.$getBlockHeightTip();
    if( height > blockHeightTip){
      return;
    }
    const blockHash = await bitcoinApi.$getBlockHash(height);
    const block = await bitcoinApi.$getBlock(blockHash);

    const transactions: TransactionExtended[] = [];

    const txIds: string[] = await bitcoinApi.$getTxIdsForBlock(blockHash);

    const mempool = memPool.getMempool();
    let transactionsFound = 0;

    for (let i = 0; i < txIds.length; i++) {
      if (mempool[txIds[i]]) {
        transactions.push(mempool[txIds[i]]);
        transactionsFound++;
      } else if (config.MEMPOOL.BACKEND === 'esplora' || memPool.isInSync() || i === 0) {
        logger.debug(`Fetching block tx ${i} of ${txIds.length}`);
        try {
          const tx = await transactionUtils.$getTransactionExtended(txIds[i]);
          transactions.push(tx);
        } catch (e) {
          logger.debug('Error fetching block tx: ${(e instanceof Error ? e.message : e)}');
          if (i === 0) {
            throw new Error('Failed to fetch Coinbase transaction: ' + txIds[i]);
          }
        }
      }
    }

    transactions.forEach((tx) => {
      if (!tx.cpfpChecked) {
        Common.setRelativesAndGetCpfpInfo(tx, mempool);
      }
    });

    logger.debug(`${transactionsFound} of ${txIds.length} found in mempool. ${txIds.length - transactionsFound} not found.`);

    const blockExtended: BlockExtended = this.getBlockExtended( block, transactions);
    return [ blockExtended, txIds, transactions ];
  }

  private getBlockExtended(block: IEsploraApi.Block, transactions: TransactionExtended[]): BlockExtended {
    const blockExtended: BlockExtended = Object.assign({ extras: {} }, block);
    blockExtended.extras.reward = transactions[0].vout.reduce((acc, curr) => acc + curr.value, 0);
    blockExtended.extras.coinbaseTx = transactionUtils.stripCoinbaseTransaction(transactions[0]);

    const transactionsTmp = [...transactions];
    transactionsTmp.shift();
    transactionsTmp.sort((a, b) => b.effectiveFeePerVsize - a.effectiveFeePerVsize);
    blockExtended.extras.medianFee = transactionsTmp.length > 0 ? Common.median(transactionsTmp.map((tx) => tx.effectiveFeePerVsize)) : 0;
    blockExtended.extras.feeRange = transactionsTmp.length > 0 ? Common.getFeesInRange(transactionsTmp, 8) : [0, 0];

    return blockExtended;
  }

}


export default new OEBlocks();

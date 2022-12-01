import { Promise } from 'bluebird';
import logger from '../../logger';
import opBlockHeaderService from '../service/op-block-header.service';
import { BlockHeader, BlockHeight } from './interfaces/op-energy.interface';

export class OpStatisticService {
  constructor() {}

  async calculateStatistics(blockHeight: BlockHeight, blockSpan: number) {
    const nbdrStatisticsList: number[] = [];
    try {
      await Promise.map(
        Array.from(Array(100).keys()),
        async (i) => {
          const startBlockHeight = blockHeight.value - i * blockSpan;
          const endBlockHeight = startBlockHeight - blockSpan + 1;
          try {
            const startBlock = (await opBlockHeaderService.$getBlockHeader('nbdr',
              startBlockHeight - blockSpan - i
            )) as BlockHeader;
            const endBlock = (await opBlockHeaderService.$getBlockHeader('nbdr',
              endBlockHeight - 1 - blockSpan - i
            )) as BlockHeader;
            const nbdr =
              (blockSpan * 600 * 100) /
              (startBlock.timestamp - endBlock.timestamp);

            nbdrStatisticsList.push(nbdr);
          } catch (error) {
            logger.err(`Error while calculating nbdr ${error}`);
            throw new Error('Error while calculating nbdr');
          }
        },
        {
          concurrency: 5,
        }
      );
      const length = nbdrStatisticsList.length;
      const mean = nbdrStatisticsList.reduce((a, b) => a + b) / length;
      return {
        nbdr: {
          avg: mean,
          stddev: Math.sqrt(
            nbdrStatisticsList
              .map((x) => Math.pow(x - mean, 2))
              .reduce((a, b) => a + b) /
              (length - 1)
          ),
        },
      };
    } catch (error) {
      logger.err(`Error while calculating nbdr ${error}`);
      return {
        error: 'Something went wrong',
        status: 500,
      };
    }
  }
}

export default new OpStatisticService();

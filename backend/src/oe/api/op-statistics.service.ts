import logger from '../../logger';
import oeBlockHeaderService from '../service/op-block-header.service';
import {
  BlockHeight,
  ConfirmedBlockHeight,
} from './interfaces/op-energy.interface';
import {
  NbdrStatistics,
  NbdrStatisticsError,
} from './interfaces/op-statistics.interface';
import opEnergyService from './op-energy.service';

const NUMBER_OF_BLOCK_SPANS = 100;
export class OpStatisticService {
  constructor() {}

  async calculateStatistics(
    blockHeight: BlockHeight,
    blockSpan: number
  ): Promise<NbdrStatistics | NbdrStatisticsError> {
    try {
      const nbdrStatisticsList: number[] = [];
      const confirmedBlocks = [] as ConfirmedBlockHeight[];

      try {
        // fetching block span list
        const blockSpanList = await opEnergyService.$getBlockSpanList(
          'nbdr',
          blockHeight.value,
          blockSpan,
          NUMBER_OF_BLOCK_SPANS
        );
        const currentTip = await oeBlockHeaderService.$getBlockHeightTip('nbdr');

        blockSpanList.forEach((blockNumber) =>
          confirmedBlocks.push(
            oeBlockHeaderService.verifyConfirmedBlockHeight(
              blockNumber.startBlockHeight,
              currentTip
            ),
            oeBlockHeaderService.verifyConfirmedBlockHeight(
              blockNumber.endBlockHeight,
              currentTip
            )
          )
        );

        const blockHeadersList =
          await oeBlockHeaderService.$getBlockHeadersByHeights(
            'nbdr',
            Array.from(new Set(confirmedBlocks))
          );

        for (let i = 0; i < NUMBER_OF_BLOCK_SPANS; i += 1) {
          const startBlock = blockHeadersList[i + 1];
          const endBlock = blockHeadersList[i];
          const nbdr =
            (blockSpan * 600 * 100) /
            (startBlock.timestamp - endBlock.timestamp);

          nbdrStatisticsList.push(nbdr);
        }
      } catch (error) {
        logger.err(`Error while calculating nbdr ${error}`);
        throw new Error('Error while calculating nbdr');
      }
      const mean =
        nbdrStatisticsList.reduce((a, b) => a + b) / NUMBER_OF_BLOCK_SPANS;

      return {
        nbdr: {
          avg: mean,
          stddev: Math.sqrt(
            nbdrStatisticsList.reduce((a, x) => a + Math.pow(x - mean, 2)) /
              (NUMBER_OF_BLOCK_SPANS - 1)
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

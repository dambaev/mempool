import logger from '../../logger';
import {
  BlockHeight,
  BlockSpanDetails,
} from './interfaces/op-energy.interface';
import {
  NbdrStatistics,
  NbdrStatisticsError,
} from './interfaces/op-statistics.interface';
import opBlockspanService from './op-blockspan.service';

const NUMBER_OF_BLOCK_SPANS = 100;
export class OpStatisticService {
  constructor() {}

  public async $getNbdrStatistics(
    UUID: string,
    blockHeight: BlockHeight,
    blockSpan: number
  ): Promise<NbdrStatistics | NbdrStatisticsError> {
    try {
      const blockSpanList = await opBlockspanService.$generateBlockSpanDetailedList(
        UUID, blockHeight.value - blockSpan, blockSpan, NUMBER_OF_BLOCK_SPANS
      );
      return this.calculateStatisticsWithBlockSpan(blockSpanList, blockSpan);
    } catch (e) {
      throw new Error(`${UUID} OpStatisticsService.$getNbdrStatistics: error while getting Nbdr Statistics data: ${e instanceof Error ? e.message : e}`);
    }
  }

  public calculateStatisticsWithBlockSpan(
    blockSpanList: BlockSpanDetails[],
    span: number
  ): NbdrStatistics | NbdrStatisticsError {
    try {
      const nbdrStatisticsList: number[] = [];
      blockSpanList.forEach((blockSpan) => {
        const nbdr =
          (span * 600 * 100) /
          (blockSpan.endBlock.timestamp - blockSpan.startBlock.timestamp);
  
        nbdrStatisticsList.push(nbdr);
      });
      const mean =
        nbdrStatisticsList.reduce((a, b) => a + b) / blockSpanList.length;

      return {
        nbdr: {
          avg: mean,
          stddev: Math.sqrt(
            nbdrStatisticsList.reduce((a, x) => a + Math.pow(x - mean, 2)) /
              (blockSpanList.length - 1)
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

  public calculateNbdr(span: number, toTime: number, fromTime: number): number {
    return (span * 600 * 100) / (toTime - fromTime);
  }
}

export default new OpStatisticService();

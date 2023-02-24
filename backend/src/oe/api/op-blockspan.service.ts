import opBlockHeaderService from '../service/op-block-header.service';
import bitcoinApi from '../../api/bitcoin/bitcoin-api-factory';
import opStatisticsService from './op-statistics.service';

import {
  BlockSpan,
  BlockSpanDetails,
  BlockSpanDetailsWithNbdr,
  ConfirmedBlockHeight,
} from './interfaces/op-energy.interface';
import { NbdrStatisticsError } from './interfaces/op-statistics.interface';

export class OpBlockSpanApiService {
  constructor() {}

  async $getBlockSpanList(
    UUID: string,
    endBlockHeight: number,
    span: number,
    blockSpanCount: number
  ): Promise<BlockSpan[]> {
    try {
      const blockSpanList = [] as BlockSpan[];
      const numberOfSpan = Math.min(
        Math.floor(endBlockHeight / span),
        blockSpanCount === -1 ? Number.MAX_VALUE : blockSpanCount
      );
      const currentTip = await bitcoinApi.$getBlockHeightTip();
      for (
        let i = endBlockHeight;
        blockSpanList.length < numberOfSpan;
        i -= span
      ) {
        const blockSpan = {
          startBlock: opBlockHeaderService.verifyConfirmedBlockHeight(
            i - span,
            { value: currentTip }
          ),
          endBlock: opBlockHeaderService.verifyConfirmedBlockHeight(i, {
            value: currentTip,
          }),
        };
        blockSpanList.push(blockSpan);
      }
      return blockSpanList;
    } catch (e) {
      throw new Error(
        `${UUID} OpBlockSpanApiService.$getBlockSpanList: error while generating block span list: ${
          e instanceof Error ? e.message : e
        }`
      );
    }
  }

  public async $generateBlockSpanDetailedList(
    UUID: string,
    endBlockHeight: number,
    span: number,
    blockSpanCount: number
  ): Promise<BlockSpanDetails[]> {
    try {
      const blockSpanList = await this.$getBlockSpanList(
        UUID,
        endBlockHeight,
        span,
        blockSpanCount
      );
      const confirmedBlockHeight = [] as ConfirmedBlockHeight[];
      blockSpanList.forEach((blockSpan) => {
        confirmedBlockHeight.push(blockSpan.startBlock, blockSpan.endBlock);
      });

      const blockHeadersList =
        await opBlockHeaderService.$getBlockHeadersByHeights(
          UUID,
          Array.from(new Set(confirmedBlockHeight))
        );

      let index = 0;
      const result: BlockSpanDetails[] = [];
      while (index < blockHeadersList.length - 1) {
        result.push({
          startBlock: {
            height: blockHeadersList[index].height,
            hash: blockHeadersList[index].current_block_hash,
            mediantime: blockHeadersList[index].mediantime,
            timestamp: blockHeadersList[index].timestamp,
          },
          endBlock: {
            height: blockHeadersList[++index].height,
            hash: blockHeadersList[index].current_block_hash,
            mediantime: blockHeadersList[index].mediantime,
            timestamp: blockHeadersList[index].timestamp,
          },
        });
      }
      return result;
    } catch (e) {
      throw new Error(
        `${UUID} OpBlockSpanApiService.$generateBlockSpanDetailedList: error while generating detailed block span list: ${
          e instanceof Error ? e.message : e
        }`
      );
    }
  }

  /**
   * @Params
   * UUID: For logging purpose
   * endBlockHeight: Block height of the last block span's end block
   * span: Difference between blocks in one block span
   * blockSpanCount: Number of block to generate; where -1 denotes no limit
   * withNbdrStatistics: Boolean value which denotes presence of nbdr in return value
   *
   * @Returns
   * Block span list with block header info OR
   * Block span list with block header and Nbdr statistics data OR
   * Nbdr statistics error
   *
   * @Description
   * Generates block span list with blocker header information.
   *
   * blockSpanCount -1 denotes no limit to the block span in the list.
   * Therefore it will generate all possible block span with the given values.
   *
   * If withNbdrStatistics is given as true it also includes nbdr data with each block span in the list.
   * To calculate Nbdr data we explicitly generate 100 extra block span
   * which exists before the first desired block span.
   * Once we have all the block span as a list we use sliding window algorithm
   * to get previous 100 block span associated with the current one to calculate it's Nbdr data
   *
   * @Return_Formate
   * 1. If withNbdrStatistics is false
   * [{
   *  startBlock: Block details object,
   *  endBlock: Block details object
   * }]
   *
   * 2. If withNbdrStatistics is true
   * [{
   *  startBlock: Block details object,
   *  endBlock: Block details object,
   *  nbdr: Current block span's statistics data
   * }]
   */
  public async $getBlockSpanDetailedList(
    UUID: string,
    endBlockHeight: number,
    span: number,
    blockSpanCount: number,
    withNbdrStatistics: boolean
  ): Promise<
    BlockSpanDetails[] | BlockSpanDetailsWithNbdr[] | NbdrStatisticsError
  > {
    try {
      const blockSpanDetailedList = await this.$generateBlockSpanDetailedList(
        UUID,
        endBlockHeight,
        span,
        withNbdrStatistics && blockSpanCount !== -1
          ? blockSpanCount + 100
          : blockSpanCount
      );
      if (!withNbdrStatistics) {
        return blockSpanDetailedList;
      }

      for (let index = 100; index < blockSpanDetailedList.length; index++) {
        const blockSpanListForStat = blockSpanDetailedList.slice(
          index - 100,
          index
        );
        const statistics = opStatisticsService.calculateStatisticsWithBlockSpan(
          blockSpanListForStat,
          span
        );
        if ('error' in statistics) {
          return statistics;
        }
        blockSpanDetailedList[index]['nbdr'] = {
          ...statistics.nbdr,
          value: opStatisticsService.calculateNbdr(
            span,
            blockSpanDetailedList[index].endBlock.timestamp,
            blockSpanDetailedList[index].startBlock.timestamp
          ),
        };
      }

      if (blockSpanCount === -1) {
        return blockSpanDetailedList;
      }

      return blockSpanDetailedList.slice(
        blockSpanDetailedList.length - blockSpanCount
      );
    } catch (e) {
      throw new Error(
        `${UUID} OpBlockSpanApiService.$getBlockSpanDetailedList: error while fetching detailed block span list: ${
          e instanceof Error ? e.message : e
        }`
      );
    }
  }
}

export default new OpBlockSpanApiService();

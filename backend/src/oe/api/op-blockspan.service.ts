import opBlockHeaderService from '../service/op-block-header.service';
import bitcoinApi from '../../api/bitcoin/bitcoin-api-factory';
import opStatisticsService from './op-statistics.service';

import { BlockSpan, BlockSpanDetails, BlockSpanDetailsWithNbdr, ConfirmedBlockHeight } from './interfaces/op-energy.interface';
import { NbdrStatisticsError } from './interfaces/op-statistics.interface';

export class OpBlockSpanApiService {
  constructor() {}

  public $getBlockSpanList(UUID: string, endBlockHeight: number, span: number, blockSpanCount: number): BlockSpan[] {
    try {
      const blockSpanList = [] as BlockSpan[];
      const numberOfSpan = Math.min(
        Math.floor(endBlockHeight / span),
        blockSpanCount === -1 ? Number.MAX_VALUE : blockSpanCount
      );
      for (let i = endBlockHeight; blockSpanList.length < numberOfSpan; i -= span) {
        const blockSpan = {
          startBlockHeight: i - span,
          endBlockHeight: i
        };
        blockSpanList.push(blockSpan);
      }
      return blockSpanList;
    } catch (e) {
      throw new Error(`${UUID} OpBlockSpanApiService.$getBlockSpanList: error while generating block span list: ${e instanceof Error ? e.message : e}`);
    }
  }

  private async $getConfirmedBlockSpanList(UUID: string, blockSpanList: BlockSpan[]): Promise<ConfirmedBlockHeight[]> {
    try {
      const confirmedBlocks = [] as ConfirmedBlockHeight[];
      const currentTip = await bitcoinApi.$getBlockHeightTip();

      blockSpanList
        .filter((blockSpan: BlockSpan) => (
          opBlockHeaderService.isBlockHeightVerified(blockSpan.startBlockHeight, currentTip)
          && opBlockHeaderService.isBlockHeightVerified(blockSpan.endBlockHeight, currentTip)
        ))
        .forEach((blockSpan: BlockSpan) => {
          confirmedBlocks.push(
            { value: blockSpan.startBlockHeight },
            { value: blockSpan.endBlockHeight },
          );
        });

      return confirmedBlocks;
    } catch (e) {
      throw new Error(`${UUID} OpBlockSpanApiService.$getConfirmedBlockSpanList: error while filtering confirmed block span list: ${e instanceof Error ? e.message : e}`);
    }
  }

  public async $generateBlockSpanDetailedList(UUID: string, endBlockHeight: number, span: number, blockSpanCount: number): Promise<BlockSpanDetails[]> {
    try {
      const blockSpanList = this.$getBlockSpanList(
        UUID,
        endBlockHeight,
        span,
        blockSpanCount
      );
      const confirmedBlockSpanList = await this.$getConfirmedBlockSpanList(UUID, blockSpanList);
      if (!confirmedBlockSpanList.length) {
        return [];
      }

      const blockHeadersList = await opBlockHeaderService.$getBlockHeadersByHeights(
        UUID,
        Array.from(new Set(confirmedBlockSpanList))
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
            height: blockHeadersList[ ++index ].height,
            hash: blockHeadersList[index].current_block_hash,
            mediantime: blockHeadersList[index].mediantime,
            timestamp: blockHeadersList[index].timestamp,
          },
        });
      }
      return result;
    } catch (e) {
      throw new Error(`${UUID} OpBlockSpanApiService.$generateBlockSpanDetailedList: error while generating detailed block span list: ${e instanceof Error ? e.message : e}`);
    }
  }

  public async $getBlockSpanDetailedList(
    UUID: string,
    endBlockHeight: number,
    span: number,
    blockSpanCount: number,
    withNbdrStatistics: boolean
  ): Promise<BlockSpanDetails[] | BlockSpanDetailsWithNbdr[] | NbdrStatisticsError> {
    try {
      const blockSpanDetailedList = await this.$generateBlockSpanDetailedList(
        UUID,
        endBlockHeight,
        span,
        withNbdrStatistics && blockSpanCount !== -1 ? blockSpanCount + 100 : blockSpanCount
      );
      if (!withNbdrStatistics) {
        return blockSpanDetailedList;
      }

      for (let index = 100; index < blockSpanDetailedList.length; index++) {
        const blockSpanListForStat = blockSpanDetailedList.slice(index - 100, index);
        const statistics = opStatisticsService.calculateStatisticsWithBlockSpan(blockSpanListForStat, span);
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

      return blockSpanDetailedList.slice(blockSpanDetailedList.length - blockSpanCount);
    } catch (e) {
      throw new Error(`${UUID} OpBlockSpanApiService.$getBlockSpanDetailedList: error while fetching detailed block span list: ${e instanceof Error ? e.message : e}`);
    }
  }
}

export default new OpBlockSpanApiService();

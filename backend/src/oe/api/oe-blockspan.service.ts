import opBlockHeaderService from '../service/op-block-header.service';
import bitcoinApi from '../../api/bitcoin/bitcoin-api-factory';

import {
  BlockSpan,
  BlockSpanDetails,
  ConfirmedBlockHeight,
} from './interfaces/op-energy.interface';
import { calculateNbdr } from '../util/helper';

export class OeBlockSpanApiService {
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
        `${UUID} OeBlockSpanApiService.$getBlockSpanList: error while generating block span list: ${
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
        const startBlock = blockHeadersList[index];
        const endBlock = blockHeadersList[++index];
        result.push({
          startBlock: {
            height: startBlock.height,
            hash: startBlock.current_block_hash,
            mediantime: startBlock.mediantime,
            timestamp: startBlock.timestamp,
          },
          endBlock: {
            height: endBlock.height,
            hash: endBlock.current_block_hash,
            mediantime: endBlock.mediantime,
            timestamp: endBlock.timestamp,
          },
          nbdr: {
            value: calculateNbdr(
              span,
              endBlock.timestamp,
              startBlock.timestamp
            )
          },
        });
      }
      return result;
    } catch (e) {
      throw new Error(
        `${UUID} OeBlockSpanApiService.$generateBlockSpanDetailedList: error while generating detailed block span list: ${
          e instanceof Error ? e.message : e
        }`
      );
    }
  }

  /**
   * @Params
   * UUID: For tracking request
   * endBlockHeight: for where the blockSpans list should end
   * span: Difference between blocks in one block span
   * blockSpanCount: Number of blockspan to generate based on below formula
   *  Math.min(
        Math.floor(endBlockHeight / span),
        blockSpanCount === -1 ? Number.MAX_VALUE : blockSpanCount
      );
   * withNbdrStatistics: Boolean value which denotes presence of nbdr stats(avg, stddev) in return value
   *
   * @Returns
   * Block span list with block header details and nbdr value for block span
   * Nbdr statistics error
   */
  public async $getBlockSpanDetailedList(
    UUID: string,
    endBlockHeight: number,
    span: number,
    blockSpanCount: number,
    withNbdrStatistics: boolean
  ): Promise<
    BlockSpanDetails[]
  > {
    try {
      const blockSpanDetailedList = await this.$generateBlockSpanDetailedList(
        UUID,
        endBlockHeight,
        span,
        blockSpanCount
      );
      return blockSpanDetailedList;
      // TODO: Add nbdr statistics data to the response payload if withNbdrStatistics is true
    } catch (e) {
      throw new Error(
        `${UUID} OeBlockSpanApiService.$getBlockSpanDetailedList: error while fetching detailed block span list: ${
          e instanceof Error ? e.message : e
        }`
      );
    }
  }
}

export default new OeBlockSpanApiService();

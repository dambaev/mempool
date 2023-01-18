import logger from '../../logger';
import { BlockHeader, ConfirmedBlockHeight } from '../api/interfaces/op-energy.interface';
import { DB } from '../database';

class OpBlockHeaderRepository {
  /**
   * Save indexed block data in the database
   */
  public async $saveBlockHeaderInDatabase(
    UUID: string,
    blockHeader: BlockHeader
  ): Promise<void> {

    const {
      height,
      version,
      previous_block_hash,
      merkle_root,
      timestamp,
      difficulty,
      nonce,
      reward,
      mediantime,
      chainwork,
      current_block_hash
    } = blockHeader;

    try {
      const query = `INSERT INTO blocks(
        height, version, current_block_hash, previous_block_hash, merkle_root,
        timestamp,  difficulty, nonce, reward, mediantime, chainwork
      ) VALUE (
        ?, ?, ?, ?, ?,
        ?, ?, ?, ?, ?, ?
      )`;

      const params: (string | number)[] = [
        height, version, current_block_hash, previous_block_hash, merkle_root, timestamp, difficulty,
        nonce, reward, mediantime, chainwork
      ];

      await DB.$with_blockSpanPool(UUID, async (connection) => {
        await DB.$profile_query(UUID, connection, query, params, 'blockSpanPool.query');
      });
    } catch (error) {
      logger.err('$saveBlockHeaderInDatabase() error' + (error instanceof Error ? error.message : error));
      throw error;
    }
  }

  public async $getLatestBlockHeight(UUID: string): Promise<ConfirmedBlockHeight> {
    try {
      const query = `select height from blocks order by height desc limit 1`;
      return await DB.$with_blockSpanPool(UUID, async (connection) => {
        const [result] = await DB.$profile_query(UUID, connection, query, [], 'blockSpanPool.query');
        if( !result[0]) {
          throw new Error( `There are no confirmed blocks yet`);
        } else {
          return { value: result[0]['height']};
        }
      });
    } catch (error) {
      logger.err(`${UUID}: ERROR: Something went wrong while finding latest block height.` + error);
      throw error;
    }
  }

  public async $getBlock(UUID: string, height: ConfirmedBlockHeight): Promise<BlockHeader> {
    try {
      const query = `select * from blocks where height = ?`;
      const params: (number)[] = [
        height.value
      ];
      return await DB.$with_blockSpanPool(UUID, async (connection) => {
        const [result] = await DB.$profile_query(UUID, connection, query, params, 'blockSpanPool.query');
        return result[0];
      });
    } catch (error) {
      logger.err(`Something went wrong while finding latest block height.` + error);
      throw error;
    }
  }

  public async $getBlockHeadersByHeights(UUID: string, blockHeights: ConfirmedBlockHeight[]): Promise<BlockHeader[]> {
    try {
      const query = `select * from blocks where height in (?)`;
      const params: (number[])[] = [
        blockHeights.map(blockHeight => blockHeight.value)
      ];
      return await DB.$with_blockSpanPool(UUID, async (connection) => {
        const [result] = await DB.$profile_query(UUID, connection, query, params, 'blockSpanPool.query');
        return result as BlockHeader[];
      });
    } catch (error) {
      logger.err(`Something went wrong while finding block height range.` + error);
      throw error;
    }
  }
}

export default new OpBlockHeaderRepository();

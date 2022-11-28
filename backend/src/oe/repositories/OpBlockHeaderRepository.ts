import logger from '../../logger';
import { BlockHeader }  from '../api/interfaces/op-energy.interface';
import opEnergyApiService from '../api/op-energy.service';
import { DB } from '../database';

class OpBlockHeaderRepository {
  /**
   * Save indexed block data in the databasen
   */
  public async $saveBlockHeaderInDatabase(
    blockHeader: BlockHeader
  ): Promise<void> {
    
    const UUID = await opEnergyApiService.$generateRandomHash();

    const {
      height,
      version,
      previousBlockHash,
      merkleRoot,
      timestamp,
      difficulty,
      nonce,
      reward,
      medianTime,
      chainWork
    } = blockHeader;

    try {
      const query = `INSERT INTO blockheaders(
        height,  version, previous_block_hash, merkle_root,
        timestamp,  difficulty, nonce, reward, mediantime, chainwork
      ) VALUE (
        ?, ?, ?, ?,
        FROM_UNIXTIME(?), ?, ?, ?, ?, ?
      )`;

      const params: (string|number)[] = [
        height, version, previousBlockHash, merkleRoot, timestamp, difficulty, 
        nonce, reward, medianTime, chainWork
      ];

      await DB.$with_blockSpanPool( UUID, async (connection) => {
        await DB.$blockSpanPool_query( UUID, connection, query, params);
      });
    } catch (error) {
      logger.err('$saveBlockHeaderInDatabase() error' + (error instanceof Error ? error.message : error));
      throw error;
    }
  }

  public async $getLatestBlockHeight(): Promise<number> {
    try {
      const query = `select height from blockheaders order by height desc limit 1`;
      const UUID = await opEnergyApiService.$generateRandomHash();
      return await DB.$with_blockSpanPool( UUID, async (connection) => {
        const [result] = await DB.$blockSpanPool_query( UUID, connection, query, []);
        return result[0] ? result[0]['height'] : 0;
      });
      return 0;
    } catch(error) {
      logger.err(`Something went wrong while finding latest block height.` + error);
      throw error;
    }
  }

  public async $getBlock(height: number): Promise<BlockHeader> {
    try {
      const query = `select * from blockheaders where height = ?`;
      const params: (number)[] = [
        height
      ];
      const UUID = await opEnergyApiService.$generateRandomHash();
      return await DB.$with_blockSpanPool( UUID, async (connection) => {
        const [result] = await DB.$blockSpanPool_query( UUID, connection, query, params);
        return result[0];
      });
    } catch(error) {
      logger.err(`Something went wrong while finding latest block height.` + error);
      throw error;
    }
  }
}

export default new OpBlockHeaderRepository();

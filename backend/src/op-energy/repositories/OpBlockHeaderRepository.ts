import { DB } from '../../database';
import logger from '../../logger';
import { BlockHeader }  from '../api/interfaces/op-energy.interface';
import opEnergyApiService from '../api/op-energy.service';

class OpBlockHeaderRepository {
  /**
   * Save indexed block data in the databasen
   */
  public async $saveBlockHeaderInDatabase(
    blockHeader: BlockHeader
  ) {
    
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
    } = blockHeader;

    try {
      const query = `INSERT INTO blockheaders(
        height,  version, previous_block_hash, merkle_root,
        timestamp,  difficulty, nonce, reward
      ) VALUE (
        ?, ?, ?, ?,
        FROM_UNIXTIME(?), ?, ?, ?
      )`;

      const params: any[] = [
        height, version, previousBlockHash, merkleRoot, timestamp, difficulty, 
        nonce, reward
      ];

      await DB.$with_blockSpanPool( UUID, async (connection) => {
        await DB.$blockSpanPool_query( UUID, connection, query, params);
      });
    } catch (e) {
      logger.err('$saveBlockHeaderInDatabase() error' + (e instanceof Error ? e.message : e));
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
      throw error
    }
  }
}

export default new OpBlockHeaderRepository();

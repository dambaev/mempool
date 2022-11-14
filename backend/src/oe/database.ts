import config from '../config';
import { createPool, PoolConnection, RowDataPacket, OkPacket, ResultSetHeader, FieldPacket} from 'mysql2/promise';
import logger from '../logger';

interface PrivatePoolConnection {
  value: PoolConnection;
};
export class DB {
  static pool = createPool({
    host: config.DATABASE.HOST,
    port: config.DATABASE.PORT,
    database: config.DATABASE.DATABASE,
    user: config.DATABASE.USERNAME,
    password: config.DATABASE.PASSWORD,
    connectionLimit: 10,
    supportBigNumbers: true,
    waitForConnections: false,
  });
  private static async $accountPool_getConnection(UUID: string): Promise<PrivatePoolConnection> {
    logger.info( `${UUID} PROFILE: start: accountPool.getConnection`);
    var connection = await private_DB.accountPool.getConnection();
    private_DB.connections_count++;
    logger.info( `${UUID} PROFILE: end: accountPool.getConnection, current connections count ${private_DB.connections_count}`);
    return ({ value: connection} as PrivatePoolConnection);
  }

  public static async $accountPool_query<T extends RowDataPacket[][] | RowDataPacket[] | OkPacket | OkPacket[] | ResultSetHeader>( UUID: string, connection: PrivatePoolConnection, query: string, args: any | any[] | { [param: string]: any }): Promise<[T, FieldPacket[]]> {
    logger.info( `${UUID} PROFILE: start: accountPool.query`);
    var result = await connection.value.query<T>( query, args);
    logger.info( `${UUID} PROFILE: end: accountPool.query`);
    return result;
  }

  private static accountPool_release(UUID: string, connection: PrivatePoolConnection) {
    logger.info( `${UUID} PROFILE: start: accountPool.release`);
    connection.value.release();
    private_DB.connections_count--;
    logger.info( `${UUID} PROFILE: end: accountPool.release, current connections count ${private_DB.connections_count}`);
  }
  public static async $with_accountPool<T>(UUID: string, fn: ((conn: PrivatePoolConnection) => Promise<T>)): Promise<T> {
    const connection = await DB.$accountPool_getConnection( UUID);
    var released = false;
    try {
      const result = await fn( connection);
      DB.accountPool_release( UUID, connection);
      released = true;
      return result;
    } catch(e) {
      if( !released) {
        DB.accountPool_release( UUID, connection);
      }
      throw new Error( `${UUID} ERROR: with_accountPool: ${e instanceof Error? e.message: e}`);
    }
  }

  private static async $blockSpanPool_getConnection(UUID: string): Promise<PrivatePoolConnection> {
    logger.info( `${UUID} PROFILE: start: blockSpanPool.getConnection`);
    const connection = await private_DB.blockSpanPool.getConnection();
    private_DB.connections_count++;
    logger.info( `${UUID} PROFILE: end: blockSpanPool.getConnection, current connections count ${private_DB.connections_count}`);
    return ({ value: connection} as PrivatePoolConnection);
  }
  
  public static async $blockSpanPool_query<T extends RowDataPacket[][] | RowDataPacket[] | OkPacket | OkPacket[] | ResultSetHeader>( UUID: string, connection: PrivatePoolConnection, query: string, args: any | any[] | { [param: string]: any }): Promise<[T, FieldPacket[]]> {
    logger.info( `${UUID} PROFILE: start: blockSpanPool.query`);
    var result = await connection.value.query<T>( query, args);
    logger.info( `${UUID} PROFILE: end: blockSpanPool.query`);
    return result;
  }
  
  private static blockSpanPool_release(UUID: string, connection: PrivatePoolConnection) {
    logger.info( `${UUID} PROFILE: start: blockSpanPool.release`);
    connection.value.release();
    private_DB.connections_count--;
    logger.info( `${UUID} PROFILE: end: blockSpanPool.release, current connections count ${private_DB.connections_count}`);
  }

  public static async $with_blockSpanPool<T>(UUID: string, fn: ((conn: PrivatePoolConnection) => Promise<T>)): Promise<T> {
    const connection = await DB.$blockSpanPool_getConnection( UUID);
    var released = false;
    try {
      const result = await fn( connection);
      DB.blockSpanPool_release( UUID, connection);
      released = true;
      return result;
    } catch(e) {
      if( !released) {
        DB.blockSpanPool_release( UUID, connection);
      }
      throw new Error( `${UUID} ERROR: with_blockSpanPool: ${e instanceof Error? e.message: e}`);
    }
  }
}

class private_DB {
  static connections_count: number = 0; // for profiling purposes
  static accountPool = createPool({
    host: config.DATABASE.HOST,
    port: config.DATABASE.PORT,
    database: config.DATABASE.ACCOUNT_DATABASE,
    user: config.DATABASE.USERNAME,
    password: config.DATABASE.PASSWORD,
    connectionLimit: 10,
    supportBigNumbers: true,
    waitForConnections: false,
  });
  static blockSpanPool = createPool({
    host: config.DATABASE.HOST,
    port: config.DATABASE.PORT,
    database: config.DATABASE.BLOCK_SPANS_DATABASE,
    user: config.DATABASE.USERNAME,
    password: config.DATABASE.PASSWORD,
    connectionLimit: 10,
    supportBigNumbers: true,
    waitForConnections: false,
  });
}

export async function checkDbConnection() {
  try {
    const connection = await DB.pool.getConnection();
    logger.info('Database connection established.');
    connection.release();
  } catch (e) {
    logger.err('Could not connect to database: ' + (e instanceof Error ? e.message : e));
    process.exit(1);
  }
}

import {DB} from '../database';
import crypto from "crypto-js";
import bitcoinApi from '../../api/bitcoin/bitcoin-api-factory';
import { IEsploraApi } from '../../api/bitcoin/esplora-api.interface';
import opBlockHeaderService from '../service/op-block-header.service';
import opBlockHeaderRepository from '../repositories/OpBlockHeaderRepository';
import config from '../../config';
import logger from '../../logger';


import {
  BlockHeader,
  SlowFastGuessValue,
  BlockHeight,
  NLockTime,
  UserId,
  TimeStrikeDB,
  AlphaNumString,
  TimeStrikeId,
  AccountSecret,
  AccountToken,
  TimeStrike,
  SlowFastGuess,
  TimeStrikesHistory,
  SlowFastResult,
  BlockHash,
  RegisterResponse,
} from './interfaces/op-energy.interface';

// magic values, that should be contained by account token hash in order to pass a quick check
const AccountTokenMagic =
  [ [ 10, '0']
  , [ 30, '0']
  , [ 60, 'e']
  ] as [number, string][];

// magic values, that should be contained by account secret in order to pass a quick check
const AccountSecretMagic =
  [ [ 10, '0']
  , [ 30, 'e']
  , [ 60, 'e']
  ] as [number, string][];

export class OpEnergyApiService {
  // those arrays contains callbacks, which will be called when appropriate entity will be created
  private newTimeStrikeCallbacks: ((timeStrike: TimeStrike) => void)[] = [];
  private newTimeSlowFastGuessCallbacks: ((slowFastGues: SlowFastGuess) => void)[] = [];

  // this variable will keep blockheader as the latest confirmed block
  // the purpose mainly is to be used as cache source of data for each new websocket client
  // without this variable, we will have to query DB every time new client will be connected, which is quite wasteful
  // this variable is static because OpEnergyIndex.checkForNewConfirmedBlock() will be called without a context and thus,
  // it will call OpEnergyApiService and OpEnergyWebsocket services without context as well
  private static latestConfirmedBlockHeader: BlockHeader | undefined = undefined;

  constructor(
  ) {
  }
  // returns a string(64) which is a sha256 hash of the src + salt string
  // result contains AccountTokenMagic magic at appropriate positions
  // which is done just to be able to perform a quick check of the user's input
  // Params:
  // - src - string(64)
  // - salt - string(64)
  getHashSalt(src: string, salt: string): AccountToken {
    if (src.length < 64) {
      throw new Error("getHashSalt: src.length < 64");
    }
    if (salt.length < 64) {
      throw new Error("getHashSalt: salt.length < 64");
    }
    var rawHash = [...crypto.SHA256(src + salt).toString().slice(0, 64)];
    // set significant bytes to be able to make a dumb check later
    AccountTokenMagic.forEach( ([index,magic]) => {
      rawHash[ index ] = magic; // set specific magic for account token hash
    });
    return { accountToken: rawHash.join('').slice(0, 64)};
  }

  isAlphaNum(str: string){
    var code, i, len;

    for (i = 0, len = str.length; i < len; i++) {
      code = str.charAt(i);
      if (!((code >= '0' && code <= '9') || // numeric (0-9)
            (code >= 'A' && code <= 'Z') || // upper alpha (A-Z)
            (code >= 'a' && code <= 'z')    // lower alpha (a-z)
            )
          ) {
        return false;
      }
    }
    return true;
  }
  public verifyNLockTime( num: number): NLockTime {
    if( num < 0) {
      throw new Error('verifyNLockTime: negative');
    }
    return {
      'value': num,
    };
  }
  public verifyBlockHeight( num: number): BlockHeight {
    if( num < 0) {
      throw new Error('verifyBlockHeight: negative');
    }
    return {
      'value': num,
    };
  }

  public verifyAccountSecret( rawString: string): AccountSecret {
    if( rawString.length !== 64) {
      throw new Error('verifyAccountSecret: length');
    }
    if( !AccountSecretMagic.reduce ( ( acc, [index, magic]) => acc && rawString[ index] === magic, true)) { // quick check for magic
      throw new Error('verifyAccountSecret: header');
    }
    if( !this.isAlphaNum(rawString)) {
      throw new Error('verifyAccountSecret: alphanum');
    }
    return {
      'value': rawString,
    };
  }

  public verifyAccountToken( rawString: string): AccountToken {
    if( rawString.length !== 64) {
      throw new Error('verifyAccountToken: length');
    }
    if( !AccountTokenMagic.reduce ( ( acc, [index, magic]) => acc && rawString[ index] === magic, true)) { // quick check for magic
      throw new Error('verifyAccountToken: header');
    }
    if( !this.isAlphaNum(rawString)) {
      throw new Error('verifyAccountToken: alphanum');
    }
    return {
      'accountToken': rawString,
    };
  }

  public verifyBlockHash( rawString: string): BlockHash {
    const hash = rawString.substring( 0, 64);
    if( hash.length < 64) {
      throw new Error( `verifyBlockHash: wrong length: ${hash}`);
    }
    return {
      'value': hash,
    }
  }

  public async $getUserIdByAccountToken( UUID: string, accountToken: AccountToken): Promise<UserId> {
    // we don't want to store accountToken in DB in raw format, so we store hash of accountToken
    // this way when DB will leak into public, random people will not be able to use hashed tokens to perform any API calls
    const accountTokenHashed = this.getHashSalt( accountToken.accountToken, config.DATABASE.SECRET_SALT);
    const query = "SELECT id,display_name FROM users WHERE secret_hash=?";
    const query1 = 'UPDATE users SET last_log_time=NOW() WHERE id = ?';
    try {
      return await DB.$with_accountPool<UserId>( UUID, async (connection) => {
        const [[raw]] = await DB.$profile_query<any>( UUID, connection, query, [ accountTokenHashed.accountToken]);
        // update last_log_time field
        const _ = await DB.$profile_query<any>( UUID, connection, query1, [ raw.id]);
        return {
          'userId': raw.id,
          'userName': raw.display_name,
        }
      });
    } catch( e) {
      throw new Error( `ERROR: OpEnergyApiService.$getUserIdByAccountToken: ${e instanceof Error? e.message: e}`);
    }
  }
  public async $getUserIdByAccountTokenCreateIfMissing( UUID: string, accountToken: AccountToken): Promise<UserId> {
    try {
      return await this.$getUserIdByAccountToken( UUID, accountToken);
    } catch(e) {
      return this.$createNewUser( UUID, accountToken, this.verifyAlphaNum( 'User'));
    }
  }
  public async $createNewUser( UUID: string, accountToken: AccountToken, displayName: AlphaNumString): Promise<UserId> {
    // we don't want to store accountToken in DB in raw format, so we store hash of accountToken
    // this way when DB will leak into public, random people will not be able to use hashed tokens to perform any API calls
    const accountTokenHashed = this.getHashSalt( accountToken.accountToken, config.DATABASE.SECRET_SALT);

    const query = "INSERT INTO users (secret_hash, display_name, creation_time,last_log_time) VALUES (?,?,NOW(),NOW())";
    try {
      return await DB.$with_accountPool( UUID, async (connection) => {
        const [raw] = await DB.$profile_query<any>( UUID
                                                      , connection
                                                      , query
                                                      , [ accountTokenHashed.accountToken.slice(0,64) // secret_hash
                                                      , displayName.value.slice(0,30)
                                                      ]);
        return {
          'userId': raw.insertId,
          'userName': displayName.value.slice(0,30),
        };
      });
    } catch (e) {
      throw new Error( `ERROR: OpEnergyApiService.$createNewUser: ${ e instanceof Error? e.message: e}`);
    }
  }
  public verifyAlphaNum( rawString: string): AlphaNumString {
    if( this.isAlphaNum( rawString)) {
      return {
        'value': rawString,
      };
    } else {
      throw new Error( 'ERROR: OpEnergyApiService.verifyAlphaNum: not alpha num');
    }
  }
  public async $getTimeStrikes( UUID: string): Promise<TimeStrikeDB[]> {
    const query = 'SELECT id,block_height,nlocktime,UNIX_TIMESTAMP(creation_time) as creation_time FROM timestrikes';
    try {
      return await DB.$with_accountPool( UUID, async (connection) => {
        const [result] = await DB.$profile_query<any>( UUID, connection, query, [ ]);
        return result.map( (record) => {
          return ({
            'id': { 'value': record.id},
            'value': {
              'blockHeight': record.block_height,
              'nLockTime': record.nlocktime,
              'creationTime': record.creation_time,
            },
          } as TimeStrikeDB)
        });
      });
    } catch(e) {
      throw new Error(`OpEnergyApiService.$getTimeStrikes: failed to query DB: ${e instanceof Error? e.message : e}`);
    }
  }
  public async $addTimeStrike( UUID: string, accountToken: AccountToken, blockHeight: BlockHeight, nlocktime: NLockTime): Promise<TimeStrikeDB> {
    // ensure blockheight is in the future, ie > currentTip yet, because we can't get guess for a block height that already discovered
    const currentTip = await bitcoinApi.$getBlockHeightTip();
    if( blockHeight.value <= currentTip) {
      throw new Error( `${UUID}: ERROR: timestrike can only be created for future block height`);
    }
    const userId = await this.$getUserIdByAccountTokenCreateIfMissing( UUID, accountToken);
    const now = Math.floor( Date.now() / 1000); // unix timestamp in UTC
    const query = 'INSERT INTO timestrikes (user_id,block_height,nlocktime,creation_time) VALUES (?,?,?,FROM_UNIXTIME(?))';
    try {
      return await DB.$with_accountPool( UUID, async (connection) => {
        const [result] = await DB.$profile_query<any>( UUID, connection, query, [ userId.userId, blockHeight.value, nlocktime.value, now]);
        const timeStrike = ({
            'blockHeight': blockHeight.value,
            'nLockTime': nlocktime.value,
            'creationTime': now,
        } as TimeStrike);
        if( this.newTimeStrikeCallbacks.length) {
          this.newTimeStrikeCallbacks.forEach((cb) => cb( timeStrike ));
        }
        return ({
          'id': { 'value': result.insertId},
          'value': timeStrike,
        }) as TimeStrikeDB;
      });
    } catch(e) {
      throw new Error(`OpEnergyApiService.$addTimeStrikes: failed to query DB: ${e instanceof Error? e.message : e}`);
    }
  }
  public async $getSlowFastGuesses( UUID: string, blockHeight: BlockHeight, nlockTime: NLockTime): Promise<SlowFastGuess[]> {
    const query = 'SELECT slowfastguesses.id,timestrikes.block_height,timestrikes.nlocktime,guess,slowfastguesses.user_id,users.display_name,UNIX_TIMESTAMP(slowfastguesses.creation_time) as creation_time\
                   FROM slowfastguesses\
                   INNER JOIN timestrikes ON slowfastguesses.timestrike_id = timestrikes.id\
                   INNER JOIN users ON slowfastguesses.user_id = users.id\
                   WHERE timestrikes.block_height = ? AND timestrikes.nlocktime = ?';
    try {
      return await DB.$with_accountPool( UUID, async (connection) => {
        const [result] = await DB.$profile_query<any>( UUID, connection, query, [ blockHeight.value, nlockTime.value ]);
        return result.map( (record) => {
          return ({
            'guess': record.guess == 0? "slow" : "fast",
            'blockHeight': record.block_height,
            'nLockTime': record.nlocktime,
            'creationTime': record.creation_time,
            'userId': record.user_id,
            'userName': record.display_name,
          } as SlowFastGuess)
        });
      });
    } catch(e) {
      throw new Error(`OpEnergyApiService.$getSlowFastGuesses: failed to query DB: ${e instanceof Error? e.message : e}`);
    }
    return [];
  }
  public async $addSlowFastGuess( UUID: string, accountToken: AccountToken, blockHeight: BlockHeight, nLockTime: NLockTime, guess: SlowFastGuessValue) {
    // ensure blockheight is in the future, ie > currentTip yet, because we can't get guess for a block height that already discovered
    const currentTip = await bitcoinApi.$getBlockHeightTip();
    if( blockHeight.value <= currentTip) {
      throw new Error( `${UUID}: ERROR: slow/fast guess can only be created for future block height`);
    }
    const userId = await this.$getUserIdByAccountTokenCreateIfMissing( UUID, accountToken);
    const timestrike_id = await this.$getTimeStrikeId( UUID, blockHeight, nLockTime);
    const now = Math.floor( Date.now() / 1000); // unix timestamp in UTC
    const query = 'INSERT INTO slowfastguesses (user_id, timestrike_id, guess, creation_time) VALUES(?,?,?,FROM_UNIXTIME(?))';
    try {
      const timeSlowFastGuess = await DB.$with_accountPool( UUID, async (connection) => {
        const [result] = await DB.$profile_query<any>( UUID, connection, query, [ userId.userId, timestrike_id.value, guess.value, now]);
        return ({
          'guess': guess.value == 0 ? "slow" : "fast",
          'blockHeight': blockHeight.value,
          'nLockTime': nLockTime.value,
          'creationTime': now,
          'userId': userId.userId,
          'userName': userId.userName,
        } as SlowFastGuess);
      });
      if( this.newTimeSlowFastGuessCallbacks.length) {
        this.newTimeSlowFastGuessCallbacks.forEach((cb) => cb( timeSlowFastGuess ));
      }
      return timeSlowFastGuess;
    } catch(e) {
      throw new Error( `$addSlowFastGuess: failed to query DB: ${e instanceof Error? e.message : e}`);
    }
  }
  public async $getTimeStrikeId( UUID: string, blockHeight: BlockHeight, nLockTime: NLockTime): Promise<TimeStrikeId> {
    const query = 'SELECT id FROM timestrikes WHERE block_height = ? AND nlocktime = ?';
    try {
      return await DB.$with_accountPool( UUID, async (connection) => {
        const [[result]] = await DB.$profile_query<any>( UUID, connection, query, [ blockHeight.value, nLockTime.value ]);
        return {
          'value': result.id,
        };
      });
    } catch(e) {
      throw new Error( `$getTimeStrikeId: failed to query DB: ${e instanceof Error? e.message : e}`);
    }
  }
  public verifySlowFastGuessValue( raw: string): SlowFastGuessValue {
    if( raw !== "slow" && raw !== "fast") {
      throw new Error( 'ERROR: verifySlowFastGuessValue: wrong value');
    }
    return {
      'value': raw == "slow" ? 0 : 1,
    };
  }
  public async $updateUserDisplayName( UUID, accountToken: AccountToken, displayName: AlphaNumString): Promise<string> {
    const userId = await this.$getUserIdByAccountTokenCreateIfMissing( UUID, accountToken);
    const query = 'UPDATE users set display_name=? WHERE users.id = ?';

    try {
      return await DB.$with_accountPool( UUID, async (connection) => {
        const [result] = await DB.$profile_query<any>( UUID, connection, query, [ displayName.value.slice(0,30), userId.userId]);
        return displayName.value;
      });
    } catch(e) {
      throw new Error(`OpEnergyApiService.$updateUserDisplayName: failed to query DB: ${e instanceof Error? e.message : e}`);
    }
  }
  public async $getTimeStrikesByBlock( UUID: string, blockHeight: BlockHeight): Promise<TimeStrikeDB[]> {
    const query = 'SELECT id,block_height,nlocktime,UNIX_TIMESTAMP(creation_time) as creation_time FROM timestrikes WHERE block_height=?';

    try {
      return await DB.$with_accountPool( UUID, async (connection) => {
      const [result] = await DB.$profile_query<any>( UUID, connection, query, [ blockHeight.value ]);
        return result.map( (record) => {
          return ({
            'id': { 'value': record.id},
            'value': {
              'blockHeight': record.block_height,
              'nLockTime': record.nlocktime,
              'creationTime': record.creation_time,
            },
          } as TimeStrikeDB)
        });
      });
    } catch(e) {
      throw new Error(`OpEnergyApiService.$getTimeStrikesByBlock: failed to query DB: ${e instanceof Error? e.message : e}`);
    }
    return [];
  }
  public setNewTimeStrikeCallback( fn: (TimeStrike) => void): void {
    this.newTimeStrikeCallbacks.push(fn);
  }
  public setNewTimeSlowFastGuessCallback( fn: (SlowFastGuess) => void): void {
    this.newTimeSlowFastGuessCallbacks.push(fn);
  }
  // this procedure returns a random hash, which is a sha256 hash
  async $generateRandomHash(): Promise<string> {
    const util = require('node:util');
    const exec = util.promisify(require('node:child_process').exec);
    const { stdout, stderr } = await exec( 'dd if=/dev/urandom bs=10 count=1 | sha256sum');
    var newHashArr = [...stdout.slice(0, 64)];
    // set signature bytes in order to be able to perform a simple check of the user's input
    const newHash = newHashArr.join('');
    if( newHash.length < 64) {
      throw new Error( 'generateRandomHash: exec error: length( stdout) < 64: ' + stderr);
    }
    if( !this.isAlphaNum( newHash)) {
      throw new Error( 'generateRandomHash: generated hash is not alpha-number');
    }
    return newHash;
  }

  public async $getTimeStrikesHistory( UUID: string): Promise<TimeStrikesHistory[]> {
    const query = 'SELECT timestrikeshistory.id,user_id,users.display_name,block_height,nlocktime,mediantime,UNIX_TIMESTAMP(timestrikeshistory.creation_time) as creation_time,UNIX_TIMESTAMP(archive_time) as archive_time\
                   FROM timestrikeshistory INNER JOIN users ON timestrikeshistory.user_id = users.id';
    try {
      return await DB.$with_accountPool( UUID, async (connection) => {
        const [result] = await DB.$profile_query<any>( UUID, connection, query, [ ]);
        return result.map( (record) => {
          return ({
            'owner': record.display_name,
            'blockHeight': record.block_height,
            'nLockTime': record.nlocktime,
            'mediantime': record.mediantime,
            'creationTime': record.creation_time,
            'archiveTime': record.archive_time,
            'wrongResults': record.wrongResults,
            'rightResults': record.rightResults,
          } as TimeStrikesHistory)
        });
      });
    } catch(e) {
      throw new Error(`OpEnergyApiService.$getTimeStrikesHistory: failed to query DB: ${e instanceof Error? e.message : e}`);
    }
  }

  public async $getSlowFastResult( UUID: string, accountToken: AccountToken, blockHeight: BlockHeight, nlockTime: NLockTime): Promise<SlowFastResult | null> {
    const userId = await this.$getUserIdByAccountToken( UUID, accountToken);
    const query = 'SELECT slowfastresults.id,timestrikeshistory.block_height,timestrikeshistory.nlocktime,guess,result,slowfastresults.user_id,users.display_name,UNIX_TIMESTAMP(slowfastresults.creation_time) as creation_time\
                   FROM slowfastresults\
                   INNER JOIN timestrikeshistory ON slowfastresults.timestrikehistory_id = timestrikeshistory.id\
                   INNER JOIN users ON slowfastresults.user_id = users.id\
                   WHERE slowfastresults.user_id = ? AND timestrikeshistory.block_height = ? AND timestrikeshistory.nlocktime = ?';
    try {
      return await DB.$with_accountPool( UUID, async (connection) => {
        const [results] = await DB.$profile_query<any>( UUID, connection, query, [ userId.userId, blockHeight.value, nlockTime.value ]);
        if (results.length < 1) {
          return null;
        } else {
          let [record] = results;
          return ({
            'guess': record.guess == 0? "slow" : "fast",
            'result': record.result == 0? "wrong" : "right",
            'blockHeight': record.block_height,
            'nLockTime': record.nlocktime,
            'creationTime': record.creation_time,
          } as SlowFastResult);
        }
      });
    } catch(e) {
      throw new Error(`OpEnergyApiService.$getSlowFastResult: failed to query DB: ${e instanceof Error? e.message : e}`);
    }
  }

  // searchs strikes and slow/fast guesses under blockHeightTip - 6
  async $slowFastGamePersistOutcome( UUID: string) {
    const latestConfirmedHeight = await opBlockHeaderRepository.$getLatestBlockHeight( UUID);

    try {
      return await DB.$with_accountPool( UUID, async (connection) => {
        const [timestrikesguesses] = await DB.$profile_query<any>( UUID, connection, 'SELECT id,user_id,block_height,nlocktime,UNIX_TIMESTAMP(creation_time) as creation_time FROM timestrikes WHERE block_height <= ?', [ latestConfirmedHeight.value ]);
        for( var i = 0; i < timestrikesguesses.length; i++) {
          const confirmedBlock = { value: i}; // sql query above proves that block height i is always confirmed, so it is okay to not to use verifyConfirmedBlockHeight here
          const block = await opBlockHeaderService.$getBlockHeader( UUID, confirmedBlock);
          const [[timestrikehistory_id]] = await DB.$profile_query<any>( UUID, connection
            , 'INSERT INTO timestrikeshistory (user_id, block_height, nlocktime, mediantime, creation_time, archive_time, wrong_results, right_results) VALUES (?, ?, ?, ?, FROM_UNIXTIME(?), NOW(),0,0) returning id'
            , [ timestrikesguesses[i].user_id, timestrikesguesses[i].block_height, timestrikesguesses[i].nlocktime, block.mediantime, timestrikesguesses[i].creation_time ]
          ); // store result into separate table
          let wrong_results = 0;
          let right_results = 0;
          const [guesses] = await DB.$profile_query<any>( UUID, connection
            , 'SELECT id,user_id,timestrike_id,guess,UNIX_TIMESTAMP(creation_time) as creation_time FROM slowfastguesses WHERE timestrike_id = ?'
            , [ timestrikesguesses[i].id ]
          );
          for( var j = 0; j < guesses.length; j++) {
            var result = 0; // wrong
            if( block.mediantime <= guesses[j].nlocktime && guesses[j].guess == 1) { // guessed fast and it was actually faster
              result = 1; // right
              right_results++;
            } else
            if( block.mediantime > timestrikesguesses[i].nlocktime && guesses[j].guess == 0) { // guess slow and it was actually slower
              result = 1; // right
              right_results++;
            } else {
              wrong_results ++;
            }
            await DB.$profile_query<any>( UUID, connection
              , 'INSERT INTO slowfastresults (user_id,timestrikehistory_id,guess,result,creation_time) VALUES (?, ?, ?, ?, FROM_UNIXTIME(?))'
              , [ guesses[j].user_id
                , timestrikehistory_id.id
                , guesses[j].guess
                , result // result is being stored here
                , guesses[j].creation_time
                ]
            );
            await DB.$profile_query<any>( UUID, connection
              , 'DELETE FROM slowfastguesses WHERE id=?'
              , [ guesses[j].id
                ]
            );
          }
          await DB.$profile_query<any>( UUID, connection
            , 'UPDATE timestrikeshistory SET wrong_results = ?, right_results = ? WHERE id = ?'
            , [ wrong_results, right_results, timestrikehistory_id.id ]
          ); // persist some statistics
          await DB.$profile_query<any>( UUID, connection, 'DELETE FROM timestrikes WHERE id = ?;' , [ timestrikesguesses[i].id ]); // remove time strike guess as it now in the timestrikehistory table
        }
      });
    } catch(e) {
      throw new Error(`${UUID} OpEnergyApiService.$slowFastGamePersistOutcome: failed to query DB: ${e instanceof Error? e.message : e}`);
    }
  }

  async $persistOutcome( UUID: string) {
    // currently, it is assumed, that timestrikes and timestrikeshistory tables are only being used by slow/fast game.
    // NOTE: in the future, it maybe that other games will be using those tables. In this case, we will need to move cleanup of the timestrike table here instead of doing it in the $slowFastGamePersistOutcome
    try {
      await this.$slowFastGamePersistOutcome( UUID); // persist outcome for slow fast game
    } catch(error) { // this.$slowFastGamePersistOutcome can fail in case if block headers table haven't been filled yet
      logger.err( `${UUID}: $slowFastGamePersistOutcome ERROR: ` + error);
    }
  }

  public async $getBlockByHash( hash: BlockHash): Promise<IEsploraApi.Block> {
    // using our own block cache goes here
    return await bitcoinApi.$getBlock(hash.value);
  }

  // this procedure generates new random account secret and it's token, which is really a hash
  // both contains appropriate secret/token magics for quick checks.
  public generateAccountSecretAndToken(): [AccountSecret, AccountToken] {
    const rnd = [...Array(10)].map( _ => String.fromCharCode(Math.floor(Math.random() * 255))).join('');
    var newHashArr = [ ... crypto.HmacSHA256(rnd, config.DATABASE.SECRET_SALT).toString().slice(0, 64)];
    // secret should have special bytes in certain places to pass input verification
    AccountSecretMagic.forEach( ([index,magic]) => {
      newHashArr[ index ] = magic; // set specific magic for account token hash
    });
    const secret = newHashArr.join(''); // this value will be used to login
    const accountToken = this.getHashSalt( secret, config.DATABASE.SECRET_SALT);
    return [ this.verifyAccountSecret( secret)
           , accountToken
           ];
  }
  public async $registerNewUser( UUID: string): Promise<RegisterResponse> {
    const [secret, token] = this.generateAccountSecretAndToken();
    return { 'accountSecret': secret.value
           , 'accountToken': token.accountToken
           };
  }

  // performs user login by given secret
  public async $loginUser( UUID: string, secret: AccountSecret): Promise< AccountToken> {
    const accountToken = this.getHashSalt( secret.value, config.DATABASE.SECRET_SALT);
    // if user's record had been persisted, then hashed token is being used as secret value
    const accountTokenStored = this.getHashSalt( accountToken.accountToken, config.DATABASE.SECRET_SALT);
    const query = 'UPDATE users SET last_log_time = NOW() WHERE secret_hash = (?)';
    try {
      await DB.$with_accountPool( UUID, async (connection) => {
        const [raw] = await DB.$profile_query<any>( UUID
                                                      , connection
                                                      , query
                                                      , [ accountTokenStored.accountToken.slice(0,64) ] // hash(hash(secret))
                                                      );
      });
    } catch (e) {
      throw new Error( `ERROR: OpEnergyApiService.$loginUser: ${ e instanceof Error? e.message: e}`);
    }
    return accountToken;
  }

  /**
   * this procedure stores given block header as latest confirmed block.
   */
  public setLatestConfirmedBlockHeader( header: BlockHeader) {
    if( !OpEnergyApiService.latestConfirmedBlockHeader || OpEnergyApiService.latestConfirmedBlockHeader.height < header.height) {
      OpEnergyApiService.latestConfirmedBlockHeader = header;
    }
  }

  /**
   * returns cache of the latest confirmed block header or undefined
   */
  public getLatestConfirmedBlockHeader() {
    return OpEnergyApiService.latestConfirmedBlockHeader;
  }

}

export default new OpEnergyApiService();

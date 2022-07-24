import logger from '../logger';
import * as WebSocket from 'ws';
import { BlockExtended, TransactionExtended, WebsocketResponse, MempoolBlock,
  OptimizedStatistic, ILoadingIndicators, IConversionRates } from '../mempool.interfaces';
import blocks from './blocks';
import memPool from './mempool';
import backendInfo from './backend-info';
import mempoolBlocks from './mempool-blocks';
import fiatConversion from './fiat-conversion';
import { Common } from './common';
import loadingIndicators from './loading-indicators';
import config from '../config';
import transactionUtils from './transaction-utils';
import {exec} from 'child_process';
import * as sha256 from 'crypto-js/sha256';

import { TimeStrike, SlowFastGuess } from './interfaces/op-energy.interface';
import opEnergyApiService from './op-energy.service';

class WebsocketHandler {
  private wss: WebSocket.Server | undefined;
  private extraInitProperties = {};

  constructor() { }

  setWebsocketServer(wss: WebSocket.Server) {
    this.wss = wss;
  }

  setExtraInitProperties(property: string, value: any) {
    this.extraInitProperties[property] = value;
  }

  setupConnectionHandling() {
    if (!this.wss) {
      throw new Error('WebSocket.Server is not set');
    }

    this.wss.on('connection', (client: WebSocket) => {
      client.on('error', logger.info);
      client.on('message', async (message: string) => {
        try {
          const parsedMessage: WebsocketResponse = JSON.parse(message);
          const response = {};

          if (parsedMessage.action === 'want') {
            client['want-blocks'] = parsedMessage.data.indexOf('blocks') > -1;
            client['want-mempool-blocks'] = parsedMessage.data.indexOf('mempool-blocks') > -1;
            client['want-live-2h-chart'] = parsedMessage.data.indexOf('live-2h-chart') > -1;
            client['want-stats'] = parsedMessage.data.indexOf('stats') > -1;
          }
          if (parsedMessage && parsedMessage['track-time-strikes'] === 'start') {
            client['want-time-strikes'] = true; // want notifications about ALL time strikes
          }
          if (parsedMessage && parsedMessage['track-time-strikes'] === 'stop') {
            client['want-time-strikes'] = null;
          }
          if (parsedMessage && parsedMessage['track-time-strike-start'] !== undefined) {
            const ts = parsedMessage['track-time-strike-start'];
            const blockHeight = opEnergyApiService.verifyBlockHeight( ts.blockHeight);
            const nLockTime = opEnergyApiService.verifyNLockTime( ts.nLockTime);
            const value = {
              'blockHeight': blockHeight.value,
              'nLockTime': nLockTime.value,
            };
            if( client['track-time-strikes'] !== undefined) { // already an array
              if( client['track-time-strikes'].indexOf( value) < 0){ // add only if it is not already exists
                client['track-time-strikes'].push( value);
              }
            } else { // create initial array
              client['track-time-strikes'] = [ value ];
            }
          }
          if (parsedMessage && parsedMessage['track-time-strike-stop'] !== undefined) {
            const ts = parsedMessage['track-time-strike-stop'];
            const blockHeight = opEnergyApiService.verifyBlockHeight( ts.blockHeight);
            const nLockTime = opEnergyApiService.verifyNLockTime( ts.nLockTime);
            const value = {
              'blockHeight': blockHeight.value,
              'nLockTime': nLockTime.value,
            };

            if ( client[ 'track-time-strikes' ]) {
              client[ 'track-time-strikes' ] = client[ 'track-time-strikes' ].filter( (element, index, array) => element.blockHeight !== value.blockHeight && element.nLockTime !== value.nLockTime);
            }
          }

          if (parsedMessage && parsedMessage['track-tx']) {
            if (/^[a-fA-F0-9]{64}$/.test(parsedMessage['track-tx'])) {
              client['track-tx'] = parsedMessage['track-tx'];
              // Client is telling the transaction wasn't found but it might have appeared before we had the time to start watching for it
              if (parsedMessage['watch-mempool']) {
                const tx = memPool.getMempool()[client['track-tx']];
                if (tx) {
                  if (config.MEMPOOL.BACKEND === 'esplora') {
                    response['tx'] = tx;
                  } else {
                    // tx.prevouts is missing from transactions when in bitcoind mode
                    try {
                      const fullTx = await transactionUtils.$getTransactionExtended(tx.txid, true);
                      response['tx'] = fullTx;
                    } catch (e) {
                      logger.debug('Error finding transaction: ' + (e instanceof Error ? e.message : e));
                    }
                  }
                } else {
                  try {
                    const fullTx = await transactionUtils.$getTransactionExtended(client['track-tx'], true);
                    response['tx'] = fullTx;
                  } catch (e) {
                    logger.debug('Error finding transaction. ' + (e instanceof Error ? e.message : e));
                    client['track-mempool-tx'] = parsedMessage['track-tx'];
                  }
                }
              }
            } else {
              client['track-tx'] = null;
            }
          }

          if (parsedMessage && parsedMessage['track-address']) {
            if (/^([a-km-zA-HJ-NP-Z1-9]{26,35}|[a-km-zA-HJ-NP-Z1-9]{80}|[a-z]{2,5}1[ac-hj-np-z02-9]{8,100}|[A-Z]{2,5}1[AC-HJ-NP-Z02-9]{8,100})$/
              .test(parsedMessage['track-address'])) {
              let matchedAddress = parsedMessage['track-address'];
              if (/^[A-Z]{2,5}1[AC-HJ-NP-Z02-9]{8,100}$/.test(parsedMessage['track-address'])) {
                matchedAddress = matchedAddress.toLowerCase();
              }
              client['track-address'] = matchedAddress;
            } else {
              client['track-address'] = null;
            }
          }

          if (parsedMessage && parsedMessage['track-asset']) {
            if (/^[a-fA-F0-9]{64}$/.test(parsedMessage['track-asset'])) {
              client['track-asset'] = parsedMessage['track-asset'];
            } else {
              client['track-asset'] = null;
            }
          }

          if (parsedMessage.action === 'init') {
            const _blocks = blocks.getBlocks().slice(-config.MEMPOOL.INITIAL_BLOCKS_AMOUNT);
            if (!_blocks) {
              return;
            }
            client.send(JSON.stringify(this.getInitData(_blocks)));
          }

          if( parsedMessage.action === 'checkAccountSecret' && parsedMessage.data.length > 0) {
            client.send( JSON.stringify( this.checkAccountSecret(parsedMessage.data[0])));
          }

          if( parsedMessage.action === 'want' && parsedMessage.data.indexOf('generatedaccounttoken') > -1) {
            this.handleGeneratedAccountToken(client);
          }

          if (parsedMessage.action === 'ping') {
            response['pong'] = true;
          }

          if (parsedMessage['track-donation'] && parsedMessage['track-donation'].length === 22) {
            client['track-donation'] = parsedMessage['track-donation'];
          }

          if (parsedMessage['track-bisq-market']) {
            if (/^[a-z]{3}_[a-z]{3}$/.test(parsedMessage['track-bisq-market'])) {
              client['track-bisq-market'] = parsedMessage['track-bisq-market'];
            } else {
              client['track-bisq-market'] = null;
            }
          }

          if (Object.keys(response).length) {
            client.send(JSON.stringify(response));
          }
        } catch (e) {
          logger.debug('Error parsing websocket message: ' + (e instanceof Error ? e.message : e));
        }
      });
    });
  }

  handleNewDonation(id: string) {
    if (!this.wss) {
      throw new Error('WebSocket.Server is not set');
    }

    this.wss.clients.forEach((client: WebSocket) => {
      if (client.readyState !== WebSocket.OPEN) {
        return;
      }
      if (client['track-donation'] === id) {
        client.send(JSON.stringify({ donationConfirmed: true }));
      }
    });
  }

  handleLoadingChanged(indicators: ILoadingIndicators) {
    if (!this.wss) {
      throw new Error('WebSocket.Server is not set');
    }

    this.wss.clients.forEach((client: WebSocket) => {
      if (client.readyState !== WebSocket.OPEN) {
        return;
      }
      client.send(JSON.stringify({ loadingIndicators: indicators }));
    });
  }

  handleNewConversionRates(conversionRates: IConversionRates) {
    if (!this.wss) {
      throw new Error('WebSocket.Server is not set');
    }

    this.wss.clients.forEach((client: WebSocket) => {
      if (client.readyState !== WebSocket.OPEN) {
        return;
      }
      client.send(JSON.stringify({ conversions: conversionRates }));
    });
  }

  getInitData(_blocks?: BlockExtended[]) {
    if (!_blocks) {
      _blocks = blocks.getBlocks().slice(-config.MEMPOOL.INITIAL_BLOCKS_AMOUNT);
    }
    return {
      'mempoolInfo': memPool.getMempoolInfo(),
      'vBytesPerSecond': memPool.getVBytesPerSecond(),
      'lastDifficultyAdjustment': blocks.getLastDifficultyAdjustmentTime(),
      'previousRetarget': blocks.getPreviousDifficultyRetarget(),
      'blocks': _blocks,
      'lastDifficultyEpochEndBlocks': blocks.getDifficultyEpochEndBlocks(),
      'conversions': fiatConversion.getConversionRates(),
      'mempool-blocks': mempoolBlocks.getMempoolBlocks(),
      'transactions': memPool.getLatestTransactions(),
      'backendInfo': backendInfo.getBackendInfo(),
      'loadingIndicators': loadingIndicators.getLoadingIndicators(),
      ...this.extraInitProperties
    };
  }

  handleNewStatistic(stats: OptimizedStatistic) {
    if (!this.wss) {
      throw new Error('WebSocket.Server is not set');
    }

    this.wss.clients.forEach((client: WebSocket) => {
      if (client.readyState !== WebSocket.OPEN) {
        return;
      }

      if (!client['want-live-2h-chart']) {
        return;
      }

      client.send(JSON.stringify({
        'live-2h-chart': stats
      }));
    });
  }

  handleMempoolChange(newMempool: { [txid: string]: TransactionExtended },
    newTransactions: TransactionExtended[], deletedTransactions: TransactionExtended[]) {
    if (!this.wss) {
      throw new Error('WebSocket.Server is not set');
    }

    mempoolBlocks.updateMempoolBlocks(newMempool);
    const mBlocks = mempoolBlocks.getMempoolBlocks();
    const mempool = memPool.getMempool();
    const mempoolInfo = memPool.getMempoolInfo();
    const vBytesPerSecond = memPool.getVBytesPerSecond();
    const rbfTransactions = Common.findRbfTransactions(newTransactions, deletedTransactions);

    for (const rbfTransaction in rbfTransactions) {
      delete mempool[rbfTransaction];
    }

    this.wss.clients.forEach(async (client: WebSocket) => {
      if (client.readyState !== WebSocket.OPEN) {
        return;
      }

      const response = {};

      if (client['want-stats']) {
        response['mempoolInfo'] = mempoolInfo;
        response['vBytesPerSecond'] = vBytesPerSecond;
        response['transactions'] = newTransactions.slice(0, 6).map((tx) => Common.stripTransaction(tx));
      }

      if (client['want-mempool-blocks']) {
        response['mempool-blocks'] = mBlocks;
      }

      if (client['track-mempool-tx']) {
        const tx = newTransactions.find((t) => t.txid === client['track-mempool-tx']);
        if (tx) {
          if (config.MEMPOOL.BACKEND !== 'esplora') {
            try {
              const fullTx = await transactionUtils.$getTransactionExtended(tx.txid, true);
              response['tx'] = fullTx;
            } catch (e) {
              logger.debug('Error finding transaction in mempool: ' + (e instanceof Error ? e.message : e));
            }
          } else {
            response['tx'] = tx;
          }
          client['track-mempool-tx'] = null;
        }
      }

      if (client['track-address']) {
        const foundTransactions: TransactionExtended[] = [];

        for (const tx of newTransactions) {
          const someVin = tx.vin.some((vin) => !!vin.prevout && vin.prevout.scriptpubkey_address === client['track-address']);
          if (someVin) {
            if (config.MEMPOOL.BACKEND !== 'esplora') {
              try {
                const fullTx = await transactionUtils.$getTransactionExtended(tx.txid, true);
                foundTransactions.push(fullTx);
              } catch (e) {
                logger.debug('Error finding transaction in mempool: ' + (e instanceof Error ? e.message : e));
              }
            } else {
              foundTransactions.push(tx);
            }
            return;
          }
          const someVout = tx.vout.some((vout) => vout.scriptpubkey_address === client['track-address']);
          if (someVout) {
            if (config.MEMPOOL.BACKEND !== 'esplora') {
              try {
                const fullTx = await transactionUtils.$getTransactionExtended(tx.txid, true);
                foundTransactions.push(fullTx);
              } catch (e) {
                logger.debug('Error finding transaction in mempool: ' + (e instanceof Error ? e.message : e));
              }
            } else {
              foundTransactions.push(tx);
            }
          }
        }

        if (foundTransactions.length) {
          response['address-transactions'] = foundTransactions;
        }
      }

      if (client['track-asset']) {
        const foundTransactions: TransactionExtended[] = [];

        newTransactions.forEach((tx) => {

          if (client['track-asset'] === Common.nativeAssetId) {
            if (tx.vin.some((vin) => !!vin.is_pegin)) {
              foundTransactions.push(tx);
              return;
            }
            if (tx.vout.some((vout) => !!vout.pegout)) {
              foundTransactions.push(tx);
            }
          } else {
            if (tx.vin.some((vin) => !!vin.issuance && vin.issuance.asset_id === client['track-asset'])) {
              foundTransactions.push(tx);
              return;
            }
            if (tx.vout.some((vout) => !!vout.asset && vout.asset === client['track-asset'])) {
              foundTransactions.push(tx);
            }
          }
        });

        if (foundTransactions.length) {
          response['address-transactions'] = foundTransactions;
        }
      }

      if (client['track-tx'] && rbfTransactions[client['track-tx']]) {
        for (const rbfTransaction in rbfTransactions) {
          if (client['track-tx'] === rbfTransaction) {
            const rbfTx = rbfTransactions[rbfTransaction];
            if (config.MEMPOOL.BACKEND !== 'esplora') {
              try {
                const fullTx = await transactionUtils.$getTransactionExtended(rbfTransaction, true);
                response['rbfTransaction'] = fullTx;
              } catch (e) {
                logger.debug('Error finding transaction in mempool: ' + (e instanceof Error ? e.message : e));
              }
            } else {
              response['rbfTransaction'] = rbfTx;
            }
            break;
          }
        }
      }

      if (Object.keys(response).length) {
        client.send(JSON.stringify(response));
      }
    });
  }

  handleNewBlock(block: BlockExtended, txIds: string[], transactions: TransactionExtended[]) {
    if (!this.wss) {
      throw new Error('WebSocket.Server is not set');
    }

    let mBlocks: undefined | MempoolBlock[];
    let matchRate = 0;
    const _memPool = memPool.getMempool();
    const _mempoolBlocks = mempoolBlocks.getMempoolBlocksWithTransactions();

    if (_mempoolBlocks[0]) {
      const matches: string[] = [];
      for (const txId of txIds) {
        if (_mempoolBlocks[0].transactionIds.indexOf(txId) > -1) {
          matches.push(txId);
        }
        delete _memPool[txId];
      }

      matchRate = Math.round((matches.length / (txIds.length - 1)) * 100);
      mempoolBlocks.updateMempoolBlocks(_memPool);
      mBlocks = mempoolBlocks.getMempoolBlocks();
    }

    block.matchRate = matchRate;

    this.wss.clients.forEach((client) => {
      if (client.readyState !== WebSocket.OPEN) {
        return;
      }

      if (!client['want-blocks']) {
        return;
      }

      const response = {
        'block': block,
        'mempoolInfo': memPool.getMempoolInfo(),
        'lastDifficultyAdjustment': blocks.getLastDifficultyAdjustmentTime(),
        'previousRetarget': blocks.getPreviousDifficultyRetarget(),
      };

      if (mBlocks && client['want-mempool-blocks']) {
        response['mempool-blocks'] = mBlocks;
      }

      if (client['track-tx'] && txIds.indexOf(client['track-tx']) > -1) {
        client['track-tx'] = null;
        response['txConfirmed'] = true;
      }

      if (client['track-address']) {
        const foundTransactions: TransactionExtended[] = [];

        transactions.forEach((tx) => {
          if (tx.vin && tx.vin.some((vin) => !!vin.prevout && vin.prevout.scriptpubkey_address === client['track-address'])) {
            foundTransactions.push(tx);
            return;
          }
          if (tx.vout && tx.vout.some((vout) => vout.scriptpubkey_address === client['track-address'])) {
            foundTransactions.push(tx);
          }
        });

        if (foundTransactions.length) {
          foundTransactions.forEach((tx) => {
            tx.status = {
              confirmed: true,
              block_height: block.height,
              block_hash: block.id,
              block_time: block.timestamp,
            };
          });

          response['block-transactions'] = foundTransactions;
        }
      }

      if (client['track-asset']) {
        const foundTransactions: TransactionExtended[] = [];

        transactions.forEach((tx) => {
          if (client['track-asset'] === Common.nativeAssetId) {
            if (tx.vin && tx.vin.some((vin) => !!vin.is_pegin)) {
              foundTransactions.push(tx);
              return;
            }
            if (tx.vout && tx.vout.some((vout) => !!vout.pegout)) {
              foundTransactions.push(tx);
            }
          } else {
            if (tx.vin && tx.vin.some((vin) => !!vin.issuance && vin.issuance.asset_id === client['track-asset'])) {
              foundTransactions.push(tx);
              return;
            }
            if (tx.vout && tx.vout.some((vout) => !!vout.asset && vout.asset === client['track-asset'])) {
              foundTransactions.push(tx);
            }
          }
        });

        if (foundTransactions.length) {
          foundTransactions.forEach((tx) => {
            tx.status = {
              confirmed: true,
              block_height: block.height,
              block_hash: block.id,
              block_time: block.timestamp,
            };
          });

          response['block-transactions'] = foundTransactions;
        }
      }

      client.send(JSON.stringify(response));
    });
  }
  // this procedure generates:
  // - a random secret, which is a sha256 hash
  // - an appropriate API token for generated random secret
  handleGeneratedAccountToken(client) {
    exec( 'dd if=/dev/urandom bs=10 count=1 | sha256sum'
        , (error, stdout, stderr) => {
          if( error) {
            logger.info( 'handleGeneratedAccountToken: exec error: ' + error);
          } else {
            var newHashArr = [...stdout.slice(0, 64)];
            // set signature bytes in order to be able to perform a simple check of the user's input
            newHashArr[10] = '0';
            newHashArr[30] = 'e';
            newHashArr[60] = 'e';
            const newHash = newHashArr.join('');
            const newAccountToken = this.getHashSalt( newHash, config.DATABASE.SECRET_SALT);
            if( this.isAlphaNum( newHash)) {
              client.send( JSON.stringify( {
                'generatedAccountSecret': newHash, // this value is being used to access account
                'generatedAccountToken': newAccountToken, // this value will be used as API token
              }));
            }
          }
        }
    );
  }
  // returns a set with the only key:
  // - 'declinedAccountSecret' in case if accountSecret haven't passed any check. The value is a short description of error
  // - 'checkedAccountToken' in case if accountSecret had passed the checks. The value is an API token which can be used for appropriate API calls
  checkAccountSecret( accountSecret: string) {
    if( accountSecret.length !== 64) {
      return {
        declinedAccountSecret: 'length',
      };
    }
    if( accountSecret[10] !== '0'
      ||accountSecret[30] !== 'e' // secret has e at this position
      ||accountSecret[60] !== 'e'
      ) {
      return {
        declinedAccountSecret: 'header',
      };
    }
    if( !this.isAlphaNum(accountSecret)) {
      return {
        declinedAccountSecret: 'alphanum',
      };
    }
    let accountToken = this.getHashSalt( accountSecret, config.DATABASE.SECRET_SALT);
    return {
      checkedAccountToken: accountToken,
    };
  }
  // returns a string(64) which is a sha256 hash of the src + salt string
  // result contains at indexes:
  // - 10: '0'
  // - 30: '0'
  // - 60: 'e'
  // which is done just to be able to perform a quick check of the user's input
  // Params:
  // - src - string(64)
  // - salt - string(64)
  getHashSalt( src: string, salt: string):string {
    if( src.length < 64) {
      throw new Error("getHashSalt: src.length < 64");
    }
    if( salt.length < 64) {
      throw new Error("getHashSalt: salt.length < 64");
    }
    var rawHash = [...sha256( src + salt).toString().slice(0,64)];
    // set significant bytes to be able to make a dumb check later
    rawHash[10] = '0';
    rawHash[30] = '0'; // token has 0 at this position
    rawHash[60] = 'e';
    return rawHash.join('').slice(0,64);
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
  // sends notifications to all the WebSockets' clients about new TimeStrike value
  handleNewTimeStrike( timeStrike: TimeStrike) {
    if (!this.wss) {
      throw new Error('WebSocket.Server is not set');
    }
    const value = {
      'blockHeight': timeStrike.blockHeight,
      'nLockTime': timeStrike.nLockTime,
    };

    this.wss.clients.forEach((client) => {
      if (client.readyState !== WebSocket.OPEN) {
        return;
      }

      if (!client['want-time-strikes']) {
        return;
      }

      client.send(JSON.stringify({ timeStrike: timeStrike} ));
    });
  }
  // sends notifications to all the WebSockets' clients about new SlowFastGuess value
  handleNewTimeSlowFastGuess( timeSlowFastGuess: SlowFastGuess) {
    if (!this.wss) {
      throw new Error('WebSocket.Server is not set');
    }
    const ts = {
      'blockHeight': timeSlowFastGuess.blockHeight,
      'nLockTime': timeSlowFastGuess.nLockTime,
    };

    this.wss.clients.forEach((client) => {
      if (client.readyState !== WebSocket.OPEN) {
        return;
      }

      if ( client['track-time-strikes'] === undefined || client['track-time-strikes'].filter( (element, index, array) => element.blockHeight === ts.blockHeight && element.nLockTime === ts.nLockTime).length < 1) {
        return;
      }
      client.send(JSON.stringify({ timeSlowFastGuess: timeSlowFastGuess} ));
    });
  }
}

export default new WebsocketHandler();

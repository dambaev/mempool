import config from '../../config';
import * as WebSocket from 'ws';
import logger from '../../logger';
import {
  BlockExtended, TransactionExtended, WebsocketResponse, MempoolBlock,
  OptimizedStatistic, ILoadingIndicators, IConversionRates
} from '../../mempool.interfaces';
import { exec } from 'child_process';
import crypto from "crypto-js";

import { TimeStrike, SlowFastGuess } from './interfaces/op-energy.interface';

import opEnergyApiService from './op-energy.service';

class OpEnergyWebsocket {

  private static wss: WebSocket.Server | undefined = undefined;

  public setUpWebsocketHandling(wss: WebSocket.Server) {
    OpEnergyWebsocket.wss = wss;
  }

  public websocketHandler(client: any, parsedMessage: WebsocketResponse) {
    if (parsedMessage && parsedMessage['track-time-strikes'] === 'start') {
      client['want-time-strikes'] = true; // want notifications about ALL time strikes
    }
    if (parsedMessage && parsedMessage['track-time-strikes'] === 'stop') {
      client['want-time-strikes'] = null;
    }
    if (parsedMessage && parsedMessage['track-time-strike-start'] !== undefined) {
      const ts = parsedMessage['track-time-strike-start'];
      const blockHeight = opEnergyApiService.verifyBlockHeight(ts.blockHeight);
      const nLockTime = opEnergyApiService.verifyNLockTime(ts.nLockTime);
      const value = {
        'blockHeight': blockHeight.value,
        'nLockTime': nLockTime.value,
      };
      if (client['track-time-strikes'] !== undefined) { // already an array
        if (client['track-time-strikes'].indexOf(value) < 0) { // add only if it is not already exists
          client['track-time-strikes'].push(value);
        }
      } else { // create initial array
        client['track-time-strikes'] = [value];
      }
    }
    if (parsedMessage && parsedMessage['track-time-strike-stop'] !== undefined) {
      const ts = parsedMessage['track-time-strike-stop'];
      const blockHeight = opEnergyApiService.verifyBlockHeight(ts.blockHeight);
      const nLockTime = opEnergyApiService.verifyNLockTime(ts.nLockTime);
      const value = {
        'blockHeight': blockHeight.value,
        'nLockTime': nLockTime.value,
      };

      if (client['track-time-strikes']) {
        client['track-time-strikes'] = client['track-time-strikes'].filter((element, index, array) => element.blockHeight !== value.blockHeight && element.nLockTime !== value.nLockTime);
      }
    }
    if (parsedMessage.action === 'checkAccountSecret' && parsedMessage.data.length > 0) {
      client.send(JSON.stringify(this.checkAccountSecret(parsedMessage.data[0])));
    }

    if (parsedMessage.action === 'want' && parsedMessage.data.indexOf('generatedaccounttoken') > -1) {
      this.handleGeneratedAccountToken(client);
    }

  }
  // this procedure generates:
  // - a random secret, which is a sha256 hash
  // - an appropriate API token for generated random secret
  handleGeneratedAccountToken(client) {
    const [secret, token] = opEnergyApiService.generateAccountSecretAndToken();
    client.send(JSON.stringify({
      'generatedAccountSecret': secret.value, // this value is being used to access account
      'generatedAccountToken': token.accountToken, // this value will be used as API token
    }));
  }
  // returns a set with the only key:
  // - 'declinedAccountSecret' in case if accountSecret haven't passed any check. The value is a short description of error
  // - 'checkedAccountToken' in case if accountSecret had passed the checks. The value is an API token which can be used for appropriate API calls
  checkAccountSecret(accountSecret: string) {
    var result;

    try {
      const secret = opEnergyApiService.verifyAccountSecret( accountSecret);
      const token = opEnergyApiService.getHashSalt(secret.value, config.DATABASE.SECRET_SALT);
      result = {
        checkedAccountToken: token.accountToken,
      };
    } catch( error) {
      result = error;
    }
    return result;
  }

  // sends notifications to all the WebSockets' clients about new TimeStrike value
  handleNewTimeStrike(timeStrike: TimeStrike) {
    if (!OpEnergyWebsocket.wss) {
      throw new Error('WebSocket.Server is not set');
    }
    const value = {
      'blockHeight': timeStrike.blockHeight,
      'nLockTime': timeStrike.nLockTime,
    };

    OpEnergyWebsocket.wss.clients.forEach((client) => {
      if (client.readyState !== WebSocket.OPEN) {
        return;
      }

      if (!client['want-time-strikes']) {
        return;
      }

      client.send(JSON.stringify({ timeStrike: timeStrike }));
    });
  }
  // sends notifications to all the WebSockets' clients about new SlowFastGuess value
  handleNewTimeSlowFastGuess(timeSlowFastGuess: SlowFastGuess) {
    if (!OpEnergyWebsocket.wss) {
      throw new Error('WebSocket.Server is not set');
    }
    const ts = {
      'blockHeight': timeSlowFastGuess.blockHeight,
      'nLockTime': timeSlowFastGuess.nLockTime,
    };

    OpEnergyWebsocket.wss.clients.forEach((client) => {
      if (client.readyState !== WebSocket.OPEN) {
        return;
      }

      if (client['track-time-strikes'] === undefined || client['track-time-strikes'].filter((element, index, array) => element.blockHeight === ts.blockHeight && element.nLockTime === ts.nLockTime).length < 1) {
        return;
      }
      client.send(JSON.stringify({ timeSlowFastGuess: timeSlowFastGuess }));
    });
  }

  /**
   * O(n)
   * this procedure sends given message to all the connected websocket clients
   */
  public sendToAllClients( msg: any) {
    if( !OpEnergyWebsocket.wss) {
      return;
    }
    OpEnergyWebsocket.wss.clients.forEach((client) => {
      if (client.readyState === WebSocket.OPEN) {
        try { // don't throw an error in case if any client's stream will be terminated
          client.send(JSON.stringify( msg));
        } catch( error) {
          // we don't need to handle it as it is expected that client will receive update within reconnection procedure
        }
      }
    });
  }

  /**
   * O(n) (where n - is initialSet's number of keys)
   * returns initial set including op-energy related data
   */
  public getInitData( initialSet: any): any {
    const newestConfirmedBlock = opEnergyApiService.getLatestConfirmedBlockHeader(); // get block header from cache
    return {
      'newest-confirmed-block': newestConfirmedBlock,
      ...initialSet
    };
  }

}

export default new OpEnergyWebsocket();

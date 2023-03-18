import * as WebSocket from 'ws';
import { Express, Request, Response, NextFunction, Application } from 'express';
import opEnergyRoutes from './api/routes';
import logger from '../logger';
import { BlockExtended, TransactionExtended, WebsocketResponse } from '../mempool.interfaces';
import { BlockHeader } from './api/interfaces/op-energy.interface';
import blocks from '../api/blocks';

import opEnergyApiService from './api/op-energy.service';
import opEnergyWebsocket from './api/websocket';
import opBlockHeaderService from './service/op-block-header.service';

class OpEnergyIndex {

  /**
   * this procedure checks if given block header can be considered as the new latest confirmed block header and thus
   * it CAN call handleNewConfirmedBlock() procedure
   */
  public static async checkForNewConfirmedBlock( block: BlockHeader) {
    const previousLatestConfirmedBlock = opEnergyApiService.getLatestConfirmedBlockHeader();
    if( !previousLatestConfirmedBlock || previousLatestConfirmedBlock.height < block.height) { // we don't want to resend data that had been already sent through network before
      opEnergyApiService.setLatestConfirmedBlockHeader( block); // update
      await OpEnergyIndex.handleNewConfirmedBlock( block);
    }
  }

  public async setUpHttpApiRoutes( app: Application) {
    opEnergyRoutes.setUpHttpApiRoutes( app);
    try {
      const latestConfirmedBlockHeader = await opBlockHeaderService.$syncOlderBlockHeader('init');
      await OpEnergyIndex.checkForNewConfirmedBlock( latestConfirmedBlockHeader);
    } catch( e) {
      logger.warn( 'opBlockHeaderService.$syncOlderBlockHeader failed, maybe there is no confirmed blocks yet' + e);
    }
  }

  public setUpWebsocketHandling( wss: WebSocket.Server) {
    opEnergyWebsocket.setUpWebsocketHandling( wss);
    blocks.setNewBlockCallback( this.handleNewUnconfirmedBlock);
  }

  public async runMainUpdateLoop() {
  }

  async handleNewUnconfirmedBlock( block: BlockExtended, txIds: string[], transactions: TransactionExtended[]) {
    try {
      const latestConfirmedBlockHeader = await opBlockHeaderService.$syncOlderBlockHeader('handleNewUnconfirmedBlock callback', block.height);

      await OpEnergyIndex.checkForNewConfirmedBlock( latestConfirmedBlockHeader);
    } catch(e) {
      logger.warn( 'opBlockHeaderService.$syncOlderBlockHeader failed, maybe there is no confirmed block yet: ' + e);
    }
  }

  /**
   * this callback will called when each new confirmed block will be discovered
   */
  private static async handleNewConfirmedBlock( block: BlockHeader) {
    await opEnergyApiService.$persistOutcome( "handleNewConfirmedBlock callback");
    opEnergyWebsocket.sendToAllClients( { // send newest block header to the connected clients
      'oe-newest-confirmed-block': block
    });
  }

}

export default new OpEnergyIndex();

import { Express, Request, Response, NextFunction } from 'express';
import config from '../config';
import * as express from 'express';
import logger from '../logger';

import opEnergyApiService from './api/op-energy.service';

class OpEnergyRoutes {
  public setUpHttpApiRoutes( app: Express) {
    logger.info(`setUpHttpApiRoutes`);
    app
      .get(config.MEMPOOL.API_URL_PREFIX + 'strike/block/mediantime', this.$getTimeStrikesByBlock)
      .get(config.MEMPOOL.API_URL_PREFIX + 'strike/mediantime', this.$getTimeStrikes)
      .post(config.MEMPOOL.API_URL_PREFIX + 'strike/mediantime', this.$postTimeStrike)
      .get(config.MEMPOOL.API_URL_PREFIX + 'slowfastguess/mediantime', this.$getSlowFastGuesses)
      .post(config.MEMPOOL.API_URL_PREFIX + 'slowfastguess/mediantime', this.$postSlowFastGuess)
      .post(config.MEMPOOL.API_URL_PREFIX + 'user/displayname', this.$postUserDisplayName)
   ;
  }
  private async $getTimeStrikes(req: Request, res: Response) {
    const UUID = await opEnergyApiService.$generateRandomHash();
    try {
      logger.info( `${UUID}: PROFILE: start: $getTimeStrikes`);
      const maccount_token = req.query.account_token;
      if( typeof maccount_token === "string" ) {
        const result = await opEnergyApiService.$getTimeStrikes( UUID, opEnergyApiService.verifyAccountToken( maccount_token));
        res.json( result.map( (blocksid) => {
          return blocksid.value;
        }));
      } else {
        throw new Error( 'ERROR: req.query.account_token is not a string');
      }
    } catch(e) {
      logger.err( `ERROR: ${UUID}: OpEnergyApiService.$getTimeStrikes: ${e instanceof Error ? e.message: e}`);
      res.status(404).send(`${UUID}: ${e instanceof Error? e.message : e}`);
    }
    logger.info( `${UUID}: PROFILE: end: $getTimeStrikes`);
  }
  private async $postTimeStrike(req: Request, res: Response) {
    const UUID = await opEnergyApiService.$generateRandomHash();
    try {
      logger.info( `${UUID} PROFILE: start: $postTimeStrike`);
      const maccount_token = req.body.account_token;
      const mnlocktime = req.body.nlocktime;
      const mblock_height = req.body.block_height;

      if( typeof maccount_token !== "string") {
        throw new Error( 'ERROR: req.body.account_token is not a string');
      }
      if( typeof mnlocktime !== "number") {
        throw new Error( 'ERROR: req.body.nlocktime is not a number');
      }
      if( typeof mblock_height !== "number" ) {
        throw new Error( 'ERROR: req.body.block_height is not a number');
      }
      const result = await opEnergyApiService.$addTimeStrike( UUID, opEnergyApiService.verifyAccountToken( maccount_token), opEnergyApiService.verifyBlockHeight( mblock_height), opEnergyApiService.verifyNLockTime(mnlocktime));
      res.json( result.value);
    } catch(e) {
      logger.err( `ERROR: ${UUID}: OpEnergyApiService.$addTimeStrike: ${e instanceof Error ? e.message: e}`);
      res.status(404).send(`${UUID}: ${e instanceof Error? e.message : e}`);
    }
    logger.info( `${UUID}: PROFILE: end: $postTimeStrike`);
  }
  private async $getSlowFastGuesses(req: Request, res: Response) {
    const UUID = await opEnergyApiService.$generateRandomHash();
    try {
      logger.info( `${UUID}: PROFILE: start: $getSlowFastGuesses`);
      const maccount_token = req.query.account_token;
      const mnlocktime = Number(req.query.nlocktime);
      const mblock_height = Number(req.query.block_height);

      if( typeof maccount_token !== "string") {
        throw new Error( 'ERROR: req.query.account_token is not a string');
      }
      if( typeof mnlocktime !== "number") {
        throw new Error( 'ERROR: req.query.nlocktime is not a number');
      }
      if( typeof mblock_height !== "number" ) {
        throw new Error( 'ERROR: req.query.block_height is not a string');
      }
      const result = await opEnergyApiService.$getSlowFastGuesses( UUID, opEnergyApiService.verifyAccountToken( maccount_token), opEnergyApiService.verifyBlockHeight( mblock_height), opEnergyApiService.verifyNLockTime(mnlocktime));
      res.json( result);
    } catch(e) {
      logger.err( `ERROR: ${UUID}: OpEnergyApiService.$getSlowFastGuesses: ${e instanceof Error ? e.message: e}`);
      res.status(404).send(`${UUID}: ${e instanceof Error? e.message : e}`);
    }
    logger.info( `${UUID}: PROFILE: end: $getSlowFastGuesses`);
  }
  private async $postSlowFastGuess(req: Request, res: Response) {
    const UUID = await opEnergyApiService.$generateRandomHash();
    try {
      logger.info( `${UUID}: PROFILE: start: $postSlowFastGuess`);
      const maccount_token = req.body.account_token;
      const mnlocktime = req.body.nlocktime;
      const mblock_height = req.body.block_height;
      const mguess = req.body.guess;

      if( typeof maccount_token !== "string") {
        throw new Error( 'ERROR: req.body.account_token is not a string');
      }
      if( typeof mnlocktime !== "number") {
        throw new Error( 'ERROR: req.body.nlocktime is not a string');
      }
      if( typeof mblock_height !== "number" ) {
        throw new Error( 'ERROR: req.body.block_height is not a string');
      }
      if( typeof mguess !== "string" ) {
        throw new Error( 'ERROR: req.body.guess is not a string');
      }
      const result = await opEnergyApiService.$addSlowFastGuess( UUID, opEnergyApiService.verifyAccountToken( maccount_token), opEnergyApiService.verifyBlockHeight( mblock_height), opEnergyApiService.verifyNLockTime(mnlocktime), opEnergyApiService.verifySlowFastGuessValue( mguess));
      res.json( result);
    } catch(e) {
      logger.err( `ERROR: ${UUID}: OpEnergyApiService.$postSlowFastGuess: ${e instanceof Error ? e.message: e}`);
      res.status(404).send(`${UUID}: ${e instanceof Error? e.message : e}`);
    }
    logger.info( `${UUID}: PROFILE: end: $postSlowFastGuess`);
  }
  private async $postUserDisplayName(req: Request, res: Response) {
    const UUID = await opEnergyApiService.$generateRandomHash();
    try {
      logger.info( `${UUID}: PROFILE: start: $postUserDisplayName`);
      const maccount_token = req.body.account_token;
      const mdisplay_name = req.body.display_name;

      if( typeof maccount_token !== "string") {
        throw new Error( 'ERROR: req.body.account_token is not a string');
      }
      if( typeof mdisplay_name !== "string") {
        throw new Error( 'ERROR: req.body.display_name is not a string');
      }
      const result = await opEnergyApiService.$updateUserDisplayName( UUID, opEnergyApiService.verifyAccountToken( maccount_token), opEnergyApiService.verifyAlphaNum(mdisplay_name.slice(0,30)));
      res.json( result);
    } catch(e) {
      logger.err( `ERROR: ${UUID}: OpEnergyApiService.$postUserDisplayName: ${e instanceof Error ? e.message: e}`);
      res.status(404).send(`${UUID}: ${e instanceof Error? e.message : e}`);
    }
    logger.info( `${UUID}: PROFILE: end: $postUserDisplayName`);
  }
  private async $getTimeStrikesByBlock(req: Request, res: Response) {
    const UUID = await opEnergyApiService.$generateRandomHash();
    try {
      logger.info( `${UUID}: PROFILE: start: $getTimeStrikesByBlock`);
      const maccount_token = req.query.account_token;
      const mblock_height = Number(req.query.block_height);
      if( typeof maccount_token !== "string" ) {
        throw new Error( 'ERROR: req.query.account_token is not a string');
      }
      if( typeof mblock_height !== "number" ) {
        throw new Error( 'ERROR: req.body.block_height is not a number');
      }
      const result = await opEnergyApiService.$getTimeStrikesByBlock( UUID, opEnergyApiService.verifyAccountToken( maccount_token), opEnergyApiService.verifyBlockHeight( mblock_height));
      res.json( result.map( (blocksid) => {
        return blocksid.value;
      }));
    } catch(e) {
      logger.err( `ERROR: ${UUID}: OpEnergyApiService.$getTimeStrikesByBlock: ${e instanceof Error ? e.message: e}`);
      res.status(404).send(`${UUID}: ${e instanceof Error? e.message : e}`);
    }
    logger.info( `${UUID}: PROFILE: end: $getTimeStrikesByBlock`);
  }
};

export default new OpEnergyRoutes();
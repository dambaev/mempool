import { Injectable } from '@angular/core';
import { ReplaySubject, BehaviorSubject, Subject, fromEvent, Observable } from 'rxjs';
import { HttpParams, HttpClient } from '@angular/common/http';
import { TimeStrike, SlowFastGuess, SlowFastGuessOutcome, TimeStrikesHistory, SlowFastResult } from '../interfaces/op-energy.interface';
import { StateService } from '../../services/state.service';
import { WebsocketService } from 'src/app/services/websocket.service';

import { WebsocketResponse, IBackendInfo } from '../../interfaces/websocket.interface';
import { OpEnergyWebsocketResponse } from '../interfaces/websocket.interface';
import { BlockExtended } from 'src/app/interfaces/node-api.interface';
import { take, switchMap} from 'rxjs/operators';


@Injectable({
  providedIn: 'root'
})
export class OeStateService {
  accountTokenState: 'init' | 'checked' | 'generated' = 'init'; // this flag is being used to check if frontend should ask to generate account id hash
  $showAccountURLWarning: ReplaySubject<boolean> = new ReplaySubject<boolean>( 1); // this flag is being used to check if frontend should display WARNING message
  $accountSecret: ReplaySubject<string> = new ReplaySubject(1); // this value will only be used if user haven't specified it
  $accountToken: ReplaySubject<string> = new ReplaySubject(1); // this value is an API token
  timeStrikes$ = new ReplaySubject<TimeStrike>(1);
  timeSlowFastGuesses$ = new ReplaySubject<SlowFastGuess>(1);
  timeSlowFastGuessesOutcome$ = new ReplaySubject<SlowFastGuessOutcome>(1);
  latestReceivedBlock$ = new ReplaySubject<BlockExtended>(1); // this object will only contain the last block received from the backend. This block can be considered as the current tip
  latestReceivedBlockHeight = -1; // plain  block height of the latest recevied block. Need this value to handle the case when backend sends newly found block


  private apiBaseUrl: string; // base URL is protocol, hostname, and port
  private apiBasePath: string; // network path is /testnet, etc. or '' for mainnet
  constructor(
    private stateService: StateService,
  ) {
    this.apiBaseUrl = ''; // use relative URL by default
    if (!stateService.isBrowser) { // except when inside AU SSR process
      this.apiBaseUrl = this.stateService.env.NGINX_PROTOCOL + '://' + this.stateService.env.NGINX_HOSTNAME + ':' + this.stateService.env.NGINX_PORT;
    }
    this.apiBasePath = ''; // assume mainnet by default
    this.stateService.networkChanged$.subscribe((network) => {
      if (network === 'bisq') {
        network = '';
      }
      this.apiBasePath = network ? '/' + network : '';
    });
  }

  // callback which will be called by websocket service
  handleWebsocketResponse( websocketService: WebsocketService, response: OpEnergyWebsocketResponse) {

    if (response.blocks && response.blocks.length > 0) { // we need to know the last block received from the backend. Mempool's behavior is to walk through all the received blocks one by one, but this means, that stateService.lastestBlockHeight may not be current tip until stateService won't receive the last block from websocket service. The difference here is that we store the last block asap and this means, that this block is always the currenttip
      const block = response.blocks[response.blocks.length - 1];
      this.latestReceivedBlock$.next( block);
    }
    if (response.block && response.block.height > this.latestReceivedBlockHeight) {
      this.latestReceivedBlockHeight = response.block.height;
      this.latestReceivedBlock$.next(response.block);
    }


    if( response.declinedAccountSecret) {
      websocketService.want(['generatedaccounttoken']);
    }
    if( response.checkedAccountToken) {
      this.accountTokenState = 'checked';
      this.$accountToken.next( response.checkedAccountToken);
      this.$showAccountURLWarning.next( false);
    }
    if( response.generatedAccountSecret  && response.generatedAccountToken) {
      this.accountTokenState = 'generated';
      this.$accountSecret.next( response.generatedAccountSecret);
      this.$accountToken.next( response.generatedAccountToken);
      this.$showAccountURLWarning.next( true);
    }
    if ( response.timeStrike) {
      const ts = response.timeStrike;
      this.timeStrikes$.next( ts);
    }
    if ( response.timeSlowFastGuess) {
      const slowFastGuess = response.timeSlowFastGuess;
      this.timeSlowFastGuesses$.next( slowFastGuess);
    }
    if ( response.timeSlowFastGuessOutcome) {
      const slowFastGuessOutcome = response.timeSlowFastGuessOutcome;
      this.timeSlowFastGuessesOutcome$.next( slowFastGuessOutcome);
    }
  }

}

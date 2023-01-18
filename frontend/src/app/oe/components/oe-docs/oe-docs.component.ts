import { Component, Inject, OnInit, OnDestroy } from '@angular/core';
import {
  AfterViewInit,
  ElementRef,
  VERSION,
  ViewChild
} from '@angular/core';

import { Env, StateService } from '../../../services/state.service';
import { WebsocketService } from '../../../services/websocket.service';
import { OeStateService } from '../../services/state.service';
import { Observable, merge, of, Subscription } from 'rxjs';
import { LanguageService } from 'src/app/services/language.service';
import { EnterpriseService } from 'src/app/services/enterprise.service';
import { DOCUMENT } from '@angular/common';
import { SwaggerUIBundle, SwaggerUIStandalonePreset } from 'swagger-ui-dist';
import { HttpParams, HttpClient } from '@angular/common/http';

@Component({
  selector:  'app-oe-docs',
  templateUrl: './oe-docs.component.html',
  styleUrls: [ './oe-docs.component.scss' ],
})
export class OeDocsComponent implements OnDestroy, OnInit, AfterViewInit {
  env: Env;
  network$: Observable<string>;
  connectionState$: Observable<number>;
  navCollapsed = false;
  isMobile = window.innerWidth <= 767.98;
  officialMempoolSpace = this.stateService.env.OFFICIAL_MEMPOOL_SPACE;
  urlLanguage: string;
  subdomain = '';
  public baseUrl: string; // base URL is protocol, hostname, and port
  public basePath: string; // network path is /testnet, etc. or '' for mainnet

  swaggerDom: HTMLElement;
  spec = {};
  specSubscription: Subscription;

  constructor(
    @Inject(DOCUMENT) public document: Document,
    public stateService: StateService,
    private languageService: LanguageService,
    private enterpriseService: EnterpriseService,
    private httpClient: HttpClient,
  ) {
    this.baseUrl = document.location.protocol + '//' + document.location.host;
    this.basePath = ''; // assume mainnet by default
    this.stateService.networkChanged$.subscribe((network) => {
      if (network === 'bisq') {
        network = '';
      }
      this.basePath = network ? '/' + network : '';
    });
  }

  ngOnInit() {
    this.env = this.stateService.env;
    this.connectionState$ = this.stateService.connectionState$;
    this.network$ = merge(of(''), this.stateService.networkChanged$);
    this.urlLanguage = this.languageService.getLanguageForUrl();
    this.subdomain = this.enterpriseService.getSubdomain();
  }

  ngOnDestroy() {
    this.specSubscription.unsubscribe();
  }

  ngAfterViewInit() {
    this.swaggerDom = this.document.getElementById('swagger');
    let params = {};
    this.specSubscription = this.httpClient.get<any>(this.basePath + '/api/v1/swagger.json', {params}).subscribe( data => {
      data.basePath = this.basePath;
      SwaggerUIBundle({
        spec: data,
        domNode: this.swaggerDom,
        deepLinking: true,
        presets: [SwaggerUIBundle.presets.apis, SwaggerUIStandalonePreset],
        layout: 'StandaloneLayout'
      });
    });
  }

  ngAfterContentInit() {
  }
  collapse(): void {
    this.navCollapsed = !this.navCollapsed;
  }

  onResize(event: any) {
    this.isMobile = window.innerWidth <= 767.98;
  }
}

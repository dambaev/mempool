import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { StartComponent } from './components/start/start.component';
import { StartV2Component } from './components/start-v2/start-v2.component';
import { TransactionComponent } from './components/transaction/transaction.component';
import { BlockComponent } from './components/block/block.component';
import { ParimutuelBetComponent } from './components/parimutuel-bet/parimutuel-bet.component';
import { AddressComponent } from './components/address/address.component';
import { MasterPageComponent } from './components/master-page/master-page.component';
import { AboutComponent } from './components/about/about.component';
import { TelevisionComponent } from './components/television/television.component';
import { StatisticsComponent } from './components/statistics/statistics.component';
import { MempoolBlockComponent } from './components/mempool-block/mempool-block.component';
import { AssetComponent } from './components/asset/asset.component';
import { AssetsComponent } from './assets/assets.component';
import { StatusViewComponent } from './components/status-view/status-view.component';
import { DashboardComponent } from './dashboard/dashboard.component';
import { SetAccountIdComponent } from './components/setaccountid/setaccountid.component';
import { LatestBlocksComponent } from './components/latest-blocks/latest-blocks.component';
import { DocsComponent } from './components/docs/docs.component';
import { TermsOfServiceComponent } from './components/terms-of-service/terms-of-service.component';
import { PrivacyPolicyComponent } from './components/privacy-policy/privacy-policy.component';
import { TrademarkPolicyComponent } from './components/trademark-policy/trademark-policy.component';
import { BisqMasterPageComponent } from './components/bisq-master-page/bisq-master-page.component';
import { SponsorComponent } from './components/sponsor/sponsor.component';
import { LiquidMasterPageComponent } from './components/liquid-master-page/liquid-master-page.component';
import { PushTransactionComponent } from './components/push-transaction/push-transaction.component';
import { PoolRankingComponent } from './components/pool-ranking/pool-ranking.component';
import { ObservedBlockDetailComponent } from './components/observed-block-detail/observed-block-detail.component';
import { BlockchainObservedBlocksComponent } from './components/blockchain-observed-blocks/blockchain-observed-blocks.component';

let routes: Routes = [
  {
    path: 'account/:accountid',
    component: SetAccountIdComponent,
  },
  {
    path: '',
    component: MasterPageComponent,
    children: [
      {
        path: 'tx/push',
        component: PushTransactionComponent,
      },
      {
        path: '',
        component: StartComponent,
        children: [
          {
            path: '',
            component: DashboardComponent,
          },
          {
            path: 'bets/diffview/:ids',
            component: DashboardComponent,
          },
          {
            path: 'tx/:id',
            component: TransactionComponent
          },
          {
            path: 'block/:id',
            component: BlockComponent
          },
          {
            path: 'bet-parimutuel/:block/:nlocktime',
            component: ParimutuelBetComponent
          },
          {
            path: 'mempool-block/:id',
            component: MempoolBlockComponent
          },
        ],
      },
      {
        path: 'blocks',
        component: LatestBlocksComponent,
      },
      {
        path: 'tetris',
        component: StartV2Component,
        children: [
          {
            path: 'blocks',
            component: BlockchainObservedBlocksComponent,
          },
          {
            path: 'blocks/:id',
            component: ObservedBlockDetailComponent
          },
        ]
      },
      {
        path: 'mining/pools',
        component: PoolRankingComponent,
      },
      {
        path: 'graphs',
        component: StatisticsComponent,
      },
      {
        path: 'about',
        component: AboutComponent,
      },
      {
        path: 'docs/api/:type',
        component: DocsComponent
      },
      {
        path: 'docs/api',
        redirectTo: 'docs/api/rest'
      },
      {
        path: 'docs',
        redirectTo: 'docs/api/rest'
      },
      {
        path: 'api',
        redirectTo: 'docs/api/rest'
      },
      {
        path: 'terms-of-service',
        component: TermsOfServiceComponent
      },
      {
        path: 'privacy-policy',
        component: PrivacyPolicyComponent
      },
      {
        path: 'trademark-policy',
        component: TrademarkPolicyComponent
      },
      {
        path: 'address/:id',
        children: [],
        component: AddressComponent
      },
      {
        path: 'sponsor',
        component: SponsorComponent,
      },
    ],
  },
  {
    path: 'testnet',
    children: [
      {
        path: '',
        component: MasterPageComponent,
        children: [
          {
            path: 'tx/push',
            component: PushTransactionComponent,
          },
          {
            path: '',
            component: StartComponent,
            children: [
              {
                path: '',
                component: DashboardComponent
              },
              {
                path: 'bets/diffview/:ids',
                component: DashboardComponent,
              },
              {
                path: 'tx/:id',
                component: TransactionComponent
              },
              {
                path: 'block/:id',
                component: BlockComponent
              },
              {
                path: 'bet-parimutuel/:block/:nlocktime',
                component: ParimutuelBetComponent
              },
              {
                path: 'mempool-block/:id',
                component: MempoolBlockComponent
              },
            ],
          },
          {
            path: 'blocks',
            component: LatestBlocksComponent,
          },
          {
            path: 'tetris',
            component: StartV2Component,
            children: [
              {
                path: 'blocks',
                component: BlockchainObservedBlocksComponent,
              },
              {
                path: 'blocks/:id',
                component: ObservedBlockDetailComponent
              },
            ]
          },
          {
            path: 'mining/pools',
            component: PoolRankingComponent,
          },
          {
            path: 'graphs',
            component: StatisticsComponent,
          },
          {
            path: 'address/:id',
            children: [],
            component: AddressComponent
          },
          {
            path: 'docs/api/:type',
            component: DocsComponent
          },
          {
            path: 'docs/api',
            redirectTo: 'docs/api/rest'
          },
          {
            path: 'docs',
            redirectTo: 'docs/api/rest'
          },
          {
            path: 'api',
            redirectTo: 'docs/api/rest'
          },
        ],
      },
      {
        path: 'tv',
        component: TelevisionComponent
      },
      {
        path: 'status',
        component: StatusViewComponent
      },
      {
        path: '**',
        redirectTo: ''
      },
    ]
  },
  {
    path: 'signet',
    children: [
      {
        path: 'account/:accountid',
        component: SetAccountIdComponent,
      },
      {
        path: '',
        component: MasterPageComponent,
        children: [
          {
            path: 'tx/push',
            component: PushTransactionComponent,
          },
          {
            path: '',
            component: StartComponent,
            children: [
              {
                path: '',
                component: DashboardComponent
              },
              {
                path: 'bets/diffview/:ids',
                component: DashboardComponent,
              },
              {
                path: 'tx/:id',
                component: TransactionComponent
              },
              {
                path: 'block/:id',
                component: BlockComponent
              },
              {
                path: 'bet-parimutuel/:block/:nlocktime',
                component: ParimutuelBetComponent
              },
              {
                path: 'mempool-block/:id',
                component: MempoolBlockComponent
              },
            ],
          },
          {
            path: 'blocks',
            component: LatestBlocksComponent,
          },
          {
            path: 'tetris',
            component: StartV2Component,
            children: [
              {
                path: 'blocks',
                component: BlockchainObservedBlocksComponent,
              },
              {
                path: 'blocks/:id',
                component: ObservedBlockDetailComponent
              },
            ]
          },
          {
            path: 'mining/pools',
            component: PoolRankingComponent,
          },
          {
            path: 'graphs',
            component: StatisticsComponent,
          },
          {
            path: 'address/:id',
            children: [],
            component: AddressComponent
          },
          {
            path: 'docs/api/:type',
            component: DocsComponent
          },
          {
            path: 'docs/api',
            redirectTo: 'docs/api/rest'
          },
          {
            path: 'docs',
            redirectTo: 'docs/api/rest'
          },
          {
            path: 'api',
            redirectTo: 'docs/api/rest'
          },
        ],
      },
      {
        path: 'tv',
        component: TelevisionComponent
      },
      {
        path: 'status',
        component: StatusViewComponent
      },
      {
        path: '**',
        redirectTo: ''
      },
    ]
  },
  {
    path: 'tv',
    component: TelevisionComponent,
  },
  {
    path: 'status',
    component: StatusViewComponent
  },
  {
    path: '**',
    redirectTo: ''
  },
];

const browserWindow = window || {};
// @ts-ignore
const browserWindowEnv = browserWindow.__env || {};

if (browserWindowEnv && browserWindowEnv.BASE_MODULE === 'bisq') {
  routes = [{
    path: '',
    component: BisqMasterPageComponent,
    loadChildren: () => import('./bisq/bisq.module').then(m => m.BisqModule)
  }];
}

if (browserWindowEnv && browserWindowEnv.BASE_MODULE === 'liquid') {
  routes = [{
    path: '',
    component: LiquidMasterPageComponent,
    children: [
      {
        path: '',
        component: StartComponent,
        children: [
          {
            path: '',
            component: DashboardComponent
          },
          {
            path: 'bets/diffview/:ids',
            component: DashboardComponent,
          },
          {
            path: 'tx/push',
            component: PushTransactionComponent,
          },
          {
            path: 'tx/:id',
            component: TransactionComponent
          },
          {
            path: 'block/:id',
            component: BlockComponent
          },
          {
            path: 'bet-parimutuel/:block/:nlocktime',
            component: ParimutuelBetComponent
          },
          {
            path: 'mempool-block/:id',
            component: MempoolBlockComponent
          },
        ],
      },
      {
        path: 'blocks',
        component: LatestBlocksComponent,
      },
      {
        path: 'tetris',
        component: StartV2Component,
        children: [
          {
            path: 'blocks',
            component: BlockchainObservedBlocksComponent,
          },
          {
            path: 'blocks/:id',
            component: ObservedBlockDetailComponent
          },
        ]
      },
      {
        path: 'graphs',
        component: StatisticsComponent,
      },
      {
        path: 'address/:id',
        component: AddressComponent
      },
      {
        path: 'asset/:id',
        component: AssetComponent
      },
      {
        path: 'assets',
        component: AssetsComponent,
      },
      {
        path: 'docs/api/:type',
        component: DocsComponent
      },
      {
        path: 'docs/api',
        redirectTo: 'docs/api/rest'
      },
      {
        path: 'docs',
        redirectTo: 'docs/api/rest'
      },
      {
        path: 'api',
        redirectTo: 'docs/api/rest'
      },
      {
        path: 'about',
        component: AboutComponent,
      },
      {
        path: 'terms-of-service',
        component: TermsOfServiceComponent
      },
      {
        path: 'privacy-policy',
        component: PrivacyPolicyComponent
      },
      {
        path: 'trademark-policy',
        component: TrademarkPolicyComponent
      },
      {
        path: 'sponsor',
        component: SponsorComponent,
      },
    ],
  },
  {
    path: 'testnet',
    children: [
      {
        path: 'account/:accountid',
        component: SetAccountIdComponent,
      },
      {
        path: '',
        component: LiquidMasterPageComponent,
        children: [
          {
            path: '',
            component: StartComponent,
            children: [
              {
                path: '',
                component: DashboardComponent
              },
              {
                path: 'bets/diffview/:ids',
                component: DashboardComponent,
              },
              {
                path: 'tx/push',
                component: PushTransactionComponent,
              },
              {
                path: 'tx/:id',
                component: TransactionComponent
              },
              {
                path: 'block/:id',
                component: BlockComponent
              },
              {
                path: 'bet-parimutuel/:block/:nlocktime',
                component: ParimutuelBetComponent
              },
              {
                path: 'mempool-block/:id',
                component: MempoolBlockComponent
              },
            ],
          },
          {
            path: 'blocks',
            component: LatestBlocksComponent,
          },
          {
            path: 'tetris',
            component: StartV2Component,
            children: [
              {
                path: 'blocks',
                component: BlockchainObservedBlocksComponent,
              },
              {
                path: 'blocks/:id',
                component: ObservedBlockDetailComponent
              },
            ]
          },
          {
            path: 'graphs',
            component: StatisticsComponent,
          },
          {
            path: 'address/:id',
            component: AddressComponent
          },
          {
            path: 'asset/:id',
            component: AssetComponent
          },
          {
            path: 'assets',
            component: AssetsComponent,
          },
          {
            path: 'docs/api/:type',
            component: DocsComponent
          },
          {
            path: 'docs/api',
            redirectTo: 'docs/api/rest'
          },
          {
            path: 'docs',
            redirectTo: 'docs/api/rest'
          },
          {
            path: 'api',
            redirectTo: 'docs/api/rest'
          },
          {
            path: 'about',
            component: AboutComponent,
          },
          {
            path: 'terms-of-service',
            component: TermsOfServiceComponent
          },
          {
            path: 'privacy-policy',
            component: PrivacyPolicyComponent
          },
          {
            path: 'trademark-policy',
            component: TrademarkPolicyComponent
          },
          {
            path: 'sponsor',
            component: SponsorComponent,
          },
        ],
      },
      {
        path: 'tv',
        component: TelevisionComponent
      },
      {
        path: 'status',
        component: StatusViewComponent
      },
    ]
  },
  {
    path: 'tv',
    component: TelevisionComponent
  },
  {
    path: 'status',
    component: StatusViewComponent
  },
  {
    path: '**',
    redirectTo: ''
  }];
}

@NgModule({
  imports: [RouterModule.forRoot(routes, {
    initialNavigation: 'enabled',
    scrollPositionRestoration: 'enabled',
    anchorScrolling: 'enabled'
})],
  exports: [RouterModule]
})
export class AppRoutingModule { }


import { Component, OnInit, OnDestroy } from '@angular/core';
import { Location } from '@angular/common';
import { ActivatedRoute, ParamMap, Router } from '@angular/router';
import { switchMap, catchError, map, take } from 'rxjs/operators';
import { Block, Transaction } from '../../../interfaces/electrs.interface';
import { combineLatest, Observable, of, Subscription } from 'rxjs';
import { StateService } from '../../../services/state.service';
import { SeoService } from 'src/app/services/seo.service';
import { WebsocketService } from 'src/app/services/websocket.service';
import { RelativeUrlPipe } from 'src/app/shared/pipes/relative-url/relative-url.pipe';
import { NgbModal } from '@ng-bootstrap/ng-bootstrap';
import { TimeStrike, NavigationObject } from 'src/app/oe/interfaces/op-energy.interface';
import { OpEnergyApiService } from 'src/app/oe/services/op-energy.service';
import { BlockTypes, ArrowDirections, BlockParts } from '../../types/constant';

@Component({
  selector: 'app-strike-summary',
  templateUrl: './strike-summary.component.html',
  styleUrls: ['./strike-summary.component.scss']
})
export class StrikeSummaryComponent implements OnInit, OnDestroy {
  network = '';
  fromBlock: Block;
  toBlock: Block;
  blockHeight: number;
  nextBlockHeight: number;
  fromBlockHash: string;
  toBlockHash: string;
  isLoadingBlock = true;
  latestBlock: Block;
  latestBlocks: Block[] = [];
  transactions: Transaction[];
  isLoadingTransactions = true;
  error: any;
  paginationMaxSize: number;
  coinbaseTx: Transaction;
  page = 1;
  itemsPerPage: number;
  txsLoadingStatus$: Observable<number>;
  showPreviousBlocklink = true;
  showNextBlocklink = true;

  subscription: Subscription;
  keyNavigationSubscription: Subscription;
  blocksSubscription: Subscription;
  networkChangedSubscription: Subscription;

  timeStrikes: TimeStrike[] = [];

  get span(): number {
    return (this.toBlock.height - this.fromBlock.height);
  }

  get timeDiff(): number {
    return this.toBlock.mediantime - this.fromBlock.mediantime;
  }

  get energyDiff(): number {
    return ((this.span * 600 - this.timeDiff) / (this.span * 600)) * 100;
  }

  get chainworkDiff(): bigint {
    if (!this.fromBlock.chainwork || !this.toBlock.chainwork) {
      return BigInt(0);
    }
    return BigInt(this.getHexValue(this.toBlock.chainwork)) - BigInt(this.getHexValue(this.fromBlock.chainwork));
  }

  get hashrate(): bigint {
    if (!this.timeDiff) {
      return BigInt(0);
    }
    return this.chainworkDiff / BigInt(this.timeDiff);
  }

  get strikeDetailLink(): string {
    return this.relativeUrlPipe.transform(`/hashstrikes/strike_detail/${this.fromBlock.height}/${this.toBlock.height}/${this.toBlock.height}/${this.fromBlock.mediantime}/${this.toBlock.mediantime}`);
  }

  constructor(
    private route: ActivatedRoute,
    private location: Location,
    private router: Router,
    private modalService: NgbModal,
    private opEnergyApiService: OpEnergyApiService,
    public stateService: StateService,
    private seoService: SeoService,
    private websocketService: WebsocketService,
    private relativeUrlPipe: RelativeUrlPipe,
  ) { }

  ngOnInit(): void {
    this.websocketService.want(['blocks', 'mempool-blocks']);
    this.paginationMaxSize = window.matchMedia('(max-width: 670px)').matches ? 3 : 5;
    this.network = this.stateService.network;
    this.itemsPerPage = this.stateService.env.ITEMS_PER_PAGE;

    this.txsLoadingStatus$ = this.route.paramMap
      .pipe(
        switchMap(() => this.stateService.loadingIndicators$),
        map((indicators) => indicators[`${BlockParts.PRE_HASH}${this.fromBlockHash}`] ? indicators[`${BlockParts.PRE_HASH}${this.fromBlockHash}`] : 0)
      );

    this.blocksSubscription = this.subscribeToService();

    this.subscription = this.route.paramMap.pipe(
      switchMap((params: ParamMap) => {
        const fromBlockHeight: number = parseInt(params.get('from'), 10);
        const toBlockHeight: number = parseInt( params.get('to'), 10);
        this.fromBlock = null;
        this.toBlock = null;
        this.page = 1;
        this.coinbaseTx = null;
        this.error = null;
        this.stateService.markBlock$.next({});

        if (history.state.data && history.state.data.blockHeight) {
          this.blockHeight = history.state.data.blockHeight;
        }

        document.body.scrollTo(0, 0);

        if (history.state.data && history.state.data.block) {
          this.blockHeight = history.state.data.block.height;
          return of([history.state.data.block, history.state.data.block]);
        }

        this.isLoadingBlock = true;

        let fromBlockInCache: Block;
        let toBlockInCache: Block;

        fromBlockInCache = this.latestBlocks.find((block: Block) => block.height === fromBlockHeight);
        toBlockInCache = this.latestBlocks.find((block: Block) => block.height === toBlockHeight);
        if (fromBlockInCache && toBlockInCache) {
          return of([fromBlockInCache, toBlockInCache]);
        }
        return combineLatest([
          this.opEnergyApiService.$getBlockByHeight(fromBlockHeight),
          this.opEnergyApiService.$getBlockByHeight(toBlockHeight).pipe(
            catchError(() => of( toBlockHeight)),
          ),
        ]);
      }),
    )
    .subscribe(([fromBlock, toBlock]: [Block, Block]) => {
      this.fromBlock = fromBlock;
      if (typeof toBlock === BlockTypes.NUMBER) {
        this.toBlock = {
          ...this.fromBlock,
          height: +toBlock,
        };
      } else {
        this.toBlock = toBlock;
      }
      this.blockHeight = fromBlock.height;
      this.nextBlockHeight = fromBlock.height + 1;
      this.setNextAndPreviousBlockLink();

      this.seoService.setTitle($localize`:@@block.component.browser-title:Block ${fromBlock.height}:BLOCK_HEIGHT:: ${fromBlock.id}:BLOCK_ID:`);
      this.isLoadingBlock = false;
      if (fromBlock.coinbaseTx) {
        this.coinbaseTx = fromBlock.coinbaseTx;
      }
      this.stateService.markBlock$.next({ blockHeight: this.blockHeight });
      this.isLoadingTransactions = true;
      this.transactions = null;

      this.stateService.$accountToken.pipe(take(1)).subscribe(() => {
        this.getTimeStrikes();
      });
    }),
    (error: Error): void => {
      this.error = error;
      this.isLoadingBlock = false;
    };

    this.networkChangedSubscription = this.stateService.networkChanged$
      .subscribe((network) => this.network = network);

    this.keyNavigationSubscription = this.stateService.keyNavigation$.subscribe((event) => {
      if (this.showPreviousBlocklink && event.key === ArrowDirections.RIGHT && this.nextBlockHeight - 2 >= 0) {
        this.navigateToPreviousBlock();
      }

      if (event.key === ArrowDirections.LEFT && this.showNextBlocklink) {
        this.navigateToNextBlock();
      }

      if (event.key === ArrowDirections.LEFT) {
        this.router.navigate([this.relativeUrlPipe.transform('/mempool-block'), '0']);
      }
    });
  }

  ngOnDestroy(): void {
    this.stateService.markBlock$.next({});
    this.subscription.unsubscribe();
    this.keyNavigationSubscription.unsubscribe();
    this.blocksSubscription.unsubscribe();
    this.networkChangedSubscription.unsubscribe();
  }

  getTimeStrikes(): void {
    this.opEnergyApiService.$listTimeStrikesByBlockHeight(this.toBlock.height)
      .subscribe((timeStrikes: TimeStrike[]) => {
        this.timeStrikes = timeStrikes.map(strike => ({
          ...strike,
          elapsedTime: strike.nLockTime - this.fromBlock.mediantime
        }));
        // Manually add a strike that is higher energy just to show what happens when it doesn't boil
        const highEnergyStrike = {
          ...this.timeStrikes[0],
          nLockTime: this.toBlock.mediantime - 30
        };
        this.timeStrikes.unshift(highEnergyStrike);
      });
  }

  onResize(event: any): void {
    this.paginationMaxSize = event.target.innerWidth < 670 ? 3 : 5;
  }

  getNavigationObject(block: Block, nextBlockHeight: number): NavigationObject {
    return { state: { data: { block, blockHeight: nextBlockHeight } } };
  }

  navigateToPreviousBlock(): void {
    if (!this.fromBlock) {
      return;
    }
    const block = this.latestBlocks.find((b: Block) => b.height === this.nextBlockHeight - 2);
    this.router.navigate([this.relativeUrlPipe.transform('/hashstrikes/strike_summary/'),
      block ? block.id : this.fromBlock.previousblockhash], this.getNavigationObject(block, this.nextBlockHeight - 2));
  }

  navigateToNextBlock(): void {
    const block = this.latestBlocks.find((b: Block) => b.height === this.nextBlockHeight);
    this.router.navigate([this.relativeUrlPipe.transform('/hashstrikes/strike_summary/'),
      block ? block.id : this.nextBlockHeight], this.getNavigationObject(block, this.nextBlockHeight));
  }

  navigateToBlockByNumber(): void {
    const block = this.latestBlocks.find((b: Block) => b.height === this.blockHeight);
    this.router.navigate([this.relativeUrlPipe.transform('/hashstrikes/strike_summary/'),
      block ? block.id : this.blockHeight], this.getNavigationObject(block, this.blockHeight));
  }

  setNextAndPreviousBlockLink(): void {
    if (this.latestBlock && this.blockHeight) {
      this.showPreviousBlocklink = this.blockHeight !== 0;
      this.showNextBlocklink = !(this.latestBlock.height && this.latestBlock.height === this.blockHeight);
    }
  }

  open(content): void {
    this.modalService.open(content, {ariaLabelledBy: 'modal-basic-title'}).result;
  }

  toHHMMSS(secs: string): string {
    const sec_num = parseInt(secs, 10);
    
    const hours   = Math.floor(sec_num / 3600);
    const minutes = Math.floor((sec_num - (hours * 3600)) / 60);
    const seconds = sec_num - (hours * 3600) - (minutes * 60);
    
    const strHours = hours < 10 ? `0${hours}` : hours.toString();
    const strMinutes = minutes < 10 ? `0${minutes}` : minutes.toString();
    const strSeconds = seconds < 10 ? `0${seconds}` : seconds.toString();
    
    return `${strHours}:${strMinutes}:${strSeconds}`;
  }

  getHexValue(str: string): string {
    const arr1 = str.split('');
    const idx = arr1.findIndex(a => a !== '0');
    let hexValue = '0x';
    if (idx > -1) {
      hexValue += str.slice(idx);
    } else {
      hexValue += str;
    }
    return hexValue;
  }

  goDetail(fromBlock, strike): void {
    this.router.navigate([this.relativeUrlPipe.transform('/hashstrikes/strike_detail/'), fromBlock.height, strike.blockHeight, strike.blockHeight, strike.nLockTime, strike.creationTime]);
  }

  subscribeToService(): Subscription {
    return this.stateService.blocks$
      .subscribe(([block]) => {
        this.latestBlock = block;
        this.latestBlocks.unshift(block);
        this.latestBlocks = this.latestBlocks.slice(0, this.stateService.env.KEEP_BLOCKS_AMOUNT);
        this.setNextAndPreviousBlockLink();

        if (block.id === this.fromBlockHash) {
          this.fromBlock = block;
        }
      });
  }
}

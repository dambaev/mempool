import { Component, OnInit, OnDestroy, ViewChild, ElementRef } from '@angular/core';
import { Location } from '@angular/common';
import { ActivatedRoute, ParamMap, Router } from '@angular/router';
import { switchMap, tap, debounceTime, catchError, map, take } from 'rxjs/operators';
import { Block, Transaction, Vout } from '../../../interfaces/electrs.interface';
import { combineLatest, Observable, of, Subscription } from 'rxjs';
import { StateService } from '../../../services/state.service';
import { SeoService } from 'src/app/services/seo.service';
import { WebsocketService } from 'src/app/services/websocket.service';
import { RelativeUrlPipe } from 'src/app/shared/pipes/relative-url/relative-url.pipe';
import {NgbModal, ModalDismissReasons} from '@ng-bootstrap/ng-bootstrap';
import { EnergyNbdrStatistics, TimeStrike } from 'src/app/oe/interfaces/op-energy.interface';
import { ToastrService } from 'ngx-toastr';
import { OpEnergyApiService } from 'src/app/oe/services/op-energy.service';
import { BlockTypes } from '../../types/constant';

@Component({
  selector: 'app-energy-detail',
  templateUrl: './energy-detail.component.html',
  styleUrls: ['./energy-detail.component.scss']
})
export class EnergyDetailComponent implements OnInit, OnDestroy {
  network = '';
  fromBlock: Block;
  // Used any to populate just the block height property
  toBlock: Block | any;
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
  showStrikes: boolean;

  average: string = null;
  stddev: string = null;

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

  constructor(
    private route: ActivatedRoute,
    private location: Location,
    private router: Router,
    private modalService: NgbModal,
    private opEnergyApiService: OpEnergyApiService,
    public stateService: StateService,
    private toastr: ToastrService,
    private seoService: SeoService,
    private websocketService: WebsocketService,
    private relativeUrlPipe: RelativeUrlPipe,
  ) { }

  ngOnInit() {
    this.websocketService.want(['blocks', 'mempool-blocks']);
    this.paginationMaxSize = window.matchMedia('(max-width: 670px)').matches ? 3 : 5;
    this.network = this.stateService.network;
    this.itemsPerPage = this.stateService.env.ITEMS_PER_PAGE;

    this.txsLoadingStatus$ = this.route.paramMap
      .pipe(
        switchMap(() => this.stateService.loadingIndicators$),
        map((indicators) => indicators['blocktxs-' + this.fromBlockHash] !== undefined ? indicators['blocktxs-' + this.fromBlockHash] : 0)
      );

    this.blocksSubscription = this.stateService.blocks$
      .subscribe(([block]) => {
        this.latestBlock = block;
        this.latestBlocks.unshift(block);
        this.latestBlocks = this.latestBlocks.slice(0, this.stateService.env.KEEP_BLOCKS_AMOUNT);
        this.setNextAndPreviousBlockLink();

        if (block.id === this.fromBlockHash) {
          this.fromBlock = block;
        }
      });

    this.subscription = this.route.paramMap.pipe(
      switchMap((params: ParamMap) => {
        const fromBlockHeight: number = parseInt( params.get('from'), 10);
        const toBlockHeight: number = parseInt( params.get('to'), 10);
        this.fromBlock = undefined;
        this.toBlock = undefined;
        this.page = 1;
        this.coinbaseTx = undefined;
        this.error = undefined;
        this.stateService.markBlock$.next({});

        if (history.state.data && history.state.data.blockHeight) {
          this.blockHeight = history.state.data.blockHeight;
        }

        document.body.scrollTo(0, 0);

        if (history.state.data && history.state.data.block) {
          this.blockHeight = history.state.data.block.height;
          return of([history.state.data.block, history.state.data.block]);
        } else {
          this.isLoadingBlock = true;

          let fromBlockInCache: Block;
          let toBlockInCache: Block;

          fromBlockInCache = this.latestBlocks.find((block) => block.height === fromBlockHeight);
          toBlockInCache = this.latestBlocks.find((block) => block.height === toBlockHeight);
          if (fromBlockInCache && toBlockInCache) {
            return of([fromBlockInCache, toBlockInCache]);
          }
          return combineLatest([
            this.opEnergyApiService.$getBlockByHeight(fromBlockHeight).pipe(
              catchError(() => of(fromBlockHeight)),
            ),
            this.opEnergyApiService.$getBlockByHeight(toBlockHeight).pipe(
              catchError(() => of(toBlockHeight)),
            )
          ]);
        }
      }),
    )
    .subscribe(([fromBlock, toBlock]: [Block, Block]) => {
      this.fromBlock = fromBlock;
      if (typeof toBlock === BlockTypes.NUMBER) {
        this.toBlock = {
          height: +toBlock,
        };
      } else {
        this.toBlock = toBlock;
      }
      this.blockHeight = fromBlock.height;
      this.nextBlockHeight = fromBlock.height + 1;
      this.setNextAndPreviousBlockLink();

      this.opEnergyApiService.$getNbdrStatistics(fromBlock.height - (this.span * 100) , this.span).subscribe({
        next: (data: EnergyNbdrStatistics) => {
          this.average = data.nbdr.avg.toFixed(2);
          this.stddev = data.nbdr.stddev.toFixed(2);
        },
        error: (error) => {
          this.toastr.error('Unable to fetch Nbdr Statistics!', 'Failed!');
        },
      });

      this.seoService.setTitle($localize`:@@block.component.browser-title:Block ${fromBlock.height}:BLOCK_HEIGHT:: ${fromBlock.id}:BLOCK_ID:`);
      this.isLoadingBlock = false;
      if (fromBlock.coinbaseTx) {
        this.coinbaseTx = fromBlock.coinbaseTx;
      }
      this.stateService.markBlock$.next({ blockHeight: this.blockHeight });
      this.isLoadingTransactions = true;
      this.transactions = null;

      this.stateService.$accountToken.pipe(take(1)).subscribe(res => {
        this.getTimeStrikes();
      })
    }),
    (error) => {
      this.error = error;
      this.isLoadingBlock = false;
    };

    this.networkChangedSubscription = this.stateService.networkChanged$
      .subscribe((network) => this.network = network);

    this.keyNavigationSubscription = this.stateService.keyNavigation$.subscribe((event) => {
      if (this.showPreviousBlocklink && event.key === 'ArrowRight' && this.nextBlockHeight - 2 >= 0) {
        this.navigateToPreviousBlock();
      }
      if (event.key === 'ArrowLeft') {
        if (this.showNextBlocklink) {
          this.navigateToNextBlock();
        } else {
          this.router.navigate([this.relativeUrlPipe.transform('/mempool-block'), '0']);
        }
      }
    });
  }

  ngOnDestroy() {
    this.stateService.markBlock$.next({});
    this.subscription.unsubscribe();
    this.keyNavigationSubscription.unsubscribe();
    this.blocksSubscription.unsubscribe();
    this.networkChangedSubscription.unsubscribe();
  }

  getTimeStrikes() {
    this.opEnergyApiService.$listTimeStrikesByBlockHeight(this.toBlock.height)
      .subscribe((timeStrikes: TimeStrike[]) => {
        this.timeStrikes = timeStrikes.map(strike => ({
          ...strike,
          elapsedTime: strike.nLockTime - this.fromBlock.mediantime
        }));
        console.log(111111111, this.timeStrikes)
      });
  }

  onResize(event: any) {
    this.paginationMaxSize = event.target.innerWidth < 670 ? 3 : 5;
  }

  navigateToPreviousBlock() {
    if (!this.fromBlock) {
      return;
    }
    const block = this.latestBlocks.find((b) => b.height === this.nextBlockHeight - 2);
    this.router.navigate([this.relativeUrlPipe.transform('/hashstrikes/energy_detail/'),
      block ? block.id : this.fromBlock.previousblockhash], { state: { data: { block, blockHeight: this.nextBlockHeight - 2 } } });
  }

  navigateToNextBlock() {
    const block = this.latestBlocks.find((b) => b.height === this.nextBlockHeight);
    this.router.navigate([this.relativeUrlPipe.transform('/hashstrikes/energy_detail/'),
      block ? block.id : this.nextBlockHeight], { state: { data: { block, blockHeight: this.nextBlockHeight } } });
  }

  navigateToBlockByNumber() {
    const block = this.latestBlocks.find((b) => b.height === this.blockHeight);
    this.router.navigate([this.relativeUrlPipe.transform('/hashstrikes/energy_detail/'),
      block ? block.id : this.blockHeight], { state: { data: { block, blockHeight: this.blockHeight } } });
  }

  setNextAndPreviousBlockLink(){
    if (this.latestBlock && this.blockHeight) {
      if (this.blockHeight === 0){
        this.showPreviousBlocklink = false;
      } else {
        this.showPreviousBlocklink = true;
      }
      if (this.latestBlock.height && this.latestBlock.height === this.blockHeight) {
        this.showNextBlocklink = false;
      } else {
        this.showNextBlocklink = true;
      }
    }
  }

  open(content) {
    this.modalService.open(content, {ariaLabelledBy: 'modal-basic-title'}).result.then((result) => {
    }, (reason) => {
    });
  }

  toHHMMSS(secs) {
    let sec_num = parseInt(secs, 10); // don't forget the second param
    let hours   = Math.floor(sec_num / 3600);
    let minutes = Math.floor((sec_num - (hours * 3600)) / 60);
    let seconds = sec_num - (hours * 3600) - (minutes * 60);
    let strHours = hours.toString();
    let strMinutes = minutes.toString();
    let strSeconds = seconds.toString();
    if (hours   < 10) {strHours   = "0"+hours;}
    if (minutes < 10) {strMinutes = "0"+minutes;}
    if (seconds < 10) {strSeconds = "0"+seconds;}
    return strHours+':'+strMinutes+':'+strSeconds;
  }

  getHexValue(str) {
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

  goDetail(fromBlock, strike) {
    this.router.navigate([this.relativeUrlPipe.transform('/hashstrikes/strike_detail/'), fromBlock.height, strike.blockHeight, strike.blockHeight, strike.nLockTime, strike.creationTime]);
  }

  energyDetailLink() {
    return this.relativeUrlPipe.transform(`/hashstrikes/energy_detail/${this.fromBlock.height}/${this.toBlock.height}`);
  }
}

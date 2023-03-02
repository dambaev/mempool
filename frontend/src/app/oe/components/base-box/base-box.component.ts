import { Component, OnInit, OnDestroy, ChangeDetectionStrategy, Input } from '@angular/core';
import { StateService } from 'src/app/services/state.service';
import { ActivatedRoute, Router } from '@angular/router';
import { RelativeUrlPipe } from 'src/app/shared/pipes/relative-url/relative-url.pipe';
import { calculateNbdr, calculateTimeSpan, navigator, toHHMMSS } from 'src/app/shared/common.utils';

export const MAX_COUNT = 14;
@Component({
  selector: 'app-base-box',
  templateUrl: './base-box.component.html',
  styleUrls: ['./base-box.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class BaseBoxComponent implements OnInit, OnDestroy {
  @Input() type: 'Energy' | 'Strike' | 'Strike_Boiling' = 'Energy';
  @Input() color = 'red';
  @Input() totalIconCount: number;
  @Input() fromTime: number;
  @Input() toTime: number;
  @Input() span: number;
  @Input() isUnknown: boolean;
  @Input() isDetailed: boolean;
  @Input() link: string;
  @Input() footerText = 'Time';
  maxCount = MAX_COUNT;

  get iconArray() {
    const count = this.totalIconCount > this.maxCount ? this.maxCount : this.totalIconCount;
    return count ? new Array(count).fill(1) : [];
  }

  get icon() {
    return this.type === 'Energy' ? 'fire' : this.type === 'Strike' ? 'tint' : 'cloud';
  }

  get timeSpan(): string {
    return calculateTimeSpan(this.toTime, this.fromTime);
  }

  get nbdr(): string {
    return calculateNbdr(this.span, this.toTime, this.fromTime);
  }

  constructor(
    private route: ActivatedRoute,
    private relativeUrlPipe: RelativeUrlPipe,
    public stateService: StateService,
    public router: Router
  ) { }

  ngOnInit(): void {
  }

  ngOnDestroy(): void {
  }

  navigateTo(): void {
    navigator(this.router, this.link);
  }
}

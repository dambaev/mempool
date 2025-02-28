import { Component, OnInit, OnDestroy, ChangeDetectionStrategy, Input } from '@angular/core';
import { StateService } from 'src/app/services/state.service';
import { ActivatedRoute, Router } from '@angular/router';
import { RelativeUrlPipe } from 'src/app/shared/pipes/relative-url/relative-url.pipe';
import { navigator, toHHMMSS } from 'src/app/shared/common.utils';

export const MAX_COUNT = 14;
@Component({
  selector: 'app-base-box-hor',
  templateUrl: './base-box-hor.component.html',
  styleUrls: ['./base-box-hor.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class BaseBoxHorComponent implements OnInit, OnDestroy {
  @Input() type: 'Energy' | 'Strike' | 'Strike_Boiling' = 'Energy';
  @Input() color = 'red';
  @Input() fromTime: number;
  @Input() toTime: number;
  @Input() span: number;
  @Input() link: string;
  maxCount = MAX_COUNT;

  get timeSpan() {
    return toHHMMSS(this.toTime - this.fromTime);
  }

  get nbdr(): string {
    if (!this.span || !this.toTime || !this.fromTime) return '???';
    return (600 * 100 * this.span / (this.toTime - this.fromTime)).toFixed(2);
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

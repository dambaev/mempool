import { Component, OnInit, OnDestroy, ChangeDetectionStrategy, Input } from '@angular/core';
import { StateService } from 'src/app/services/state.service';
import { ActivatedRoute, ParamMap } from '@angular/router';
import { RelativeUrlPipe } from 'src/app/shared/pipes/relative-url/relative-url.pipe';

export const MAX_COUNT = 14;
@Component({
  selector: 'app-tank-box',
  templateUrl: './tank-box.component.html',
  styleUrls: ['./tank-box.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class TankBoxComponent implements OnInit, OnDestroy {
  @Input() type: 'Energy' | 'Strike' | 'Strike_Boiling' = 'Energy';
  @Input() color = 'red';
  @Input() totalIconCount: number;
  @Input() time: string;
  @Input() isUnknown: boolean;
  maxCount = MAX_COUNT;

  get iconArray() {
    const count = this.totalIconCount > this.maxCount ? this.maxCount : this.totalIconCount;
    return count ? new Array(count).fill(1) : [];
  }

  get icon() {
    return this.type === 'Energy' ? 'fire' : this.type === 'Strike' ? 'tint' : 'cloud';
  }

  constructor(
    private route: ActivatedRoute,
    private relativeUrlPipe: RelativeUrlPipe,
    public stateService: StateService,
  ) { }

  ngOnInit(): void {
  }

  ngOnDestroy(): void {
  }
}

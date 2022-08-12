import { Component, OnInit, OnDestroy, ChangeDetectionStrategy, Input } from '@angular/core';
import { StateService } from 'src/app/services/state.service';
import { ActivatedRoute, ParamMap } from '@angular/router';
import { RelativeUrlPipe } from 'src/app/shared/pipes/relative-url/relative-url.pipe';
import { Block } from '../../../interfaces/electrs.interface';

@Component({
  selector: 'app-energy',
  templateUrl: './energy.component.html',
  styleUrls: ['./energy.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class EnergyComponent implements OnInit, OnDestroy {
  @Input() fromBlock: Block;
  @Input() toBlock: Block;

  get span(): number {
    return (this.toBlock.height - this.fromBlock.height);
  }

  get timeDiff(): number {
    return this.toBlock.mediantime - this.fromBlock.mediantime;
  }

  get energyDiff(): number {
    return ((this.span * 600 - this.timeDiff) / (this.span * 600)) * 100;
  }

  get totalIconCount() {
    let count = Math.round((6 + this.energyDiff / 5) / 2);
    if (count < 0) {
      count = 0;
    }
    return count;
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

  toHHMMSS(secs) {
    if (!secs) {
      return '??:??:??';
    }
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
}

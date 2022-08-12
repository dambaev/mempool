import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { TetrisEnergyBlockComponent } from './tetris-energy-block.component';

describe('TetrisEnergyBlockComponent', () => {
  let component: TetrisEnergyBlockComponent;
  let fixture: ComponentFixture<TetrisEnergyBlockComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ TetrisEnergyBlockComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TetrisEnergyBlockComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

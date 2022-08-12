import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { TankBoxComponent } from './tank-box.component';

describe('TankBoxComponent', () => {
  let component: TankBoxComponent;
  let fixture: ComponentFixture<TankBoxComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ TankBoxComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TankBoxComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

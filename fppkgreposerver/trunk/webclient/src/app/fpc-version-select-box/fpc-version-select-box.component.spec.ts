import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { FpcVersionSelectBoxComponent } from './fpc-version-select-box.component';

describe('FpcVersionSelectBoxComponent', () => {
  let component: FpcVersionSelectBoxComponent;
  let fixture: ComponentFixture<FpcVersionSelectBoxComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ FpcVersionSelectBoxComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(FpcVersionSelectBoxComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

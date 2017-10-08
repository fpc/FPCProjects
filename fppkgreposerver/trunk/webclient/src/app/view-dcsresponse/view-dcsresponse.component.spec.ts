import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ViewDCSResponseComponent } from './view-dcsresponse.component';

describe('ViewDCSResponseComponent', () => {
  let component: ViewDCSResponseComponent;
  let fixture: ComponentFixture<ViewDCSResponseComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ ViewDCSResponseComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ViewDCSResponseComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { BuildTaskComponent } from './build-task.component';

describe('BuildTaskComponent', () => {
  let component: BuildTaskComponent;
  let fixture: ComponentFixture<BuildTaskComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ BuildTaskComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(BuildTaskComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

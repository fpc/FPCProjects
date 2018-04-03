import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { BuildTaskPageComponent } from './build-task-page.component';

describe('BuildTaskPageComponent', () => {
  let component: BuildTaskPageComponent;
  let fixture: ComponentFixture<BuildTaskPageComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ BuildTaskPageComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(BuildTaskPageComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

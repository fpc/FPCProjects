import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { RebuildRepositoryComponent } from './rebuild-repository.component';

describe('RebuildRepositoryComponent', () => {
  let component: RebuildRepositoryComponent;
  let fixture: ComponentFixture<RebuildRepositoryComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ RebuildRepositoryComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(RebuildRepositoryComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

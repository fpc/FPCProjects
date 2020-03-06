import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { PackageCommitListComponent } from './package-commit-list.component';

describe('PackageCommitListComponent', () => {
  let component: PackageCommitListComponent;
  let fixture: ComponentFixture<PackageCommitListComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ PackageCommitListComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(PackageCommitListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

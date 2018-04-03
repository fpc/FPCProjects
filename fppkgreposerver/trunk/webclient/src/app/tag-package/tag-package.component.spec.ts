import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { TagPackageComponent } from './tag-package.component';

describe('TagPackageComponent', () => {
  let component: TagPackageComponent;
  let fixture: ComponentFixture<TagPackageComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ TagPackageComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TagPackageComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

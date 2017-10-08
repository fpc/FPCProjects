import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { BuildPackageComponent } from './build-package.component';

describe('BuildPackageComponent', () => {
  let component: BuildPackageComponent;
  let fixture: ComponentFixture<BuildPackageComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ BuildPackageComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(BuildPackageComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { PackageuploadComponent } from './packageupload.component';

describe('PackageuploadComponent', () => {
  let component: PackageuploadComponent;
  let fixture: ComponentFixture<PackageuploadComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ PackageuploadComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(PackageuploadComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});

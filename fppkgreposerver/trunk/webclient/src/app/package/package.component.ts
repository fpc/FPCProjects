import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';
import { NgbModal } from '@ng-bootstrap/ng-bootstrap';
import { UploadPackageComponent } from '../upload-package/upload-package.component';
import { TagPackageComponent } from '../tag-package/tag-package.component';
import { PackageService } from '../package.service';
import { Package } from '../package';
import { Category } from '../category';
import { VersionUtils } from '../version';
import { FPCVersion } from '../fpcversion';
import { CategoryService } from '../category.service';
import { CurrentFpcversionService } from '../current-fpcversion.service';
import { OidcSecurityService } from '../auth/services/oidc.security.service';

@Component({
  selector: 'app-package',
  templateUrl: './package.component.html',
  styleUrls: ['./package.component.css']
})
export class PackageComponent implements OnInit {

  selectedFPCVersion: FPCVersion = null;
  packageVersionList: any[] = [];
  selectedVersion: any = null;
  categoryList: Category[];
  public currentPackage: Package = null;

  @Input() set package(selpackage: Package) {
    this.currentPackage = selpackage;
    this.filterPackageVersionList();
  };
  @Output() packageUpdated = new EventEmitter();
  mayEditPackage: boolean;


  constructor(
    private _modalService: NgbModal,
    private _packageService: PackageService,
    private currentFpcversionService: CurrentFpcversionService,
    private categoryService: CategoryService,
    public oidcSecurityService: OidcSecurityService) { }

  showUploadSourceDialog() {
    const modalRef = this._modalService.open(UploadPackageComponent);
    modalRef.componentInstance.package = this.currentPackage;
    modalRef.componentInstance.packageSourceUploadedEvent.subscribe(event => this.packageUpdated.emit());

    this._packageService.getPackage(this.currentPackage.name)
      .subscribe(fpcPackage => this.currentPackage = fpcPackage);
  }

  showTagDialog() {
    const modalRef = this._modalService.open(TagPackageComponent);
    modalRef.componentInstance.package = this.currentPackage;

    modalRef.result.then(modalResult => {
      if (modalResult == 'tagged') {
        this.packageUpdated.emit();
      }
    }, err => {});
  }

  approvePackage() {
    this._packageService.approvePackage(this.currentPackage.name)
      .subscribe(fpcPackage => this.packageUpdated.emit());
  }

  patchPackageCategory() {
    this._packageService.patchPackageCategory(this.currentPackage)
      .subscribe(fpcPackage => this.packageUpdated.emit());
  }

  selectVersion(version) {
    this.selectedVersion = version;
  }

  filterPackageVersionList() {
    this.packageVersionList = [];
    if ((this.currentPackage) && this.selectedFPCVersion) {
      this.packageVersionList = this.currentPackage.packageversionlist.filter(packageVersion => packageVersion.fpcversion===this.selectedFPCVersion.name);
      this.packageVersionList.sort((packageVersionA, packgageversionB) => VersionUtils.compare(packageVersionA.version, packgageversionB.version));
      if (this.packageVersionList.length > 0) {
        this.selectedVersion = this.packageVersionList[0];
      } else {
        this.selectedVersion = null;
      }
    }
  }

  ngOnInit() {
    this.currentFpcversionService.getCurrentVersion()
      .subscribe(version => {
        this.selectedFPCVersion = version;
        this.filterPackageVersionList()
       } )
    this.oidcSecurityService.getUserData().subscribe(
      (data: any) => {
        this.mayEditPackage = (data.role == "admin");
        if (this.mayEditPackage) {
          this.categoryService.getCategoryList()
            .subscribe(categoryList => { this.categoryList = categoryList } )
        }
      }
    );
  }
}

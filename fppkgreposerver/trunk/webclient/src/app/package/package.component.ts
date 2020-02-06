import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';
import { Router, NavigationEnd, Event } from '@angular/router';
import { NgbModal } from '@ng-bootstrap/ng-bootstrap';
import { ActivatedRoute } from '@angular/router';
import { UploadPackageComponent } from '../upload-package/upload-package.component';
import { TagPackageComponent } from '../tag-package/tag-package.component';
import { PackageService } from '../package.service';
import { Package } from '../package';
import { Category } from '../category';
import { VersionUtils } from '../version';
import { FPCVersion } from '../fpcversion';
import { CategoryService } from '../category.service';
import { CurrentFpcversionService } from '../current-fpcversion.service';
import { OidcSecurityService } from 'angular-auth-oidc-client';

@Component({
  selector: 'app-package',
  templateUrl: './package.component.html',
  styleUrls: ['./package.component.css']
})
export class PackageComponent implements OnInit {

  selectedFPCVersion: FPCVersion = null;
  packageVersionList: any[] = [];
  selectedVersion: any = null;
  previousTag: string = null;
  nextTag: string = null;
  categoryList: Category[];
  public currentPackage: Package = null;
  private sub: string = null;

  @Input() set package(selpackage: Package) {
    this.currentPackage = selpackage;
    this.filterPackageVersionList();
    this.updateRights();
  };
  @Output() packageUpdated = new EventEmitter();
  mayEditPackage: boolean;
  isAdmin: boolean;


  constructor(
    private _modalService: NgbModal,
    private _packageService: PackageService,
    private currentFpcversionService: CurrentFpcversionService,
    private categoryService: CategoryService,
    private route: ActivatedRoute,
    private router: Router,
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

  patchSupportCategory() {
    this._packageService.patchSupportCategory(this.currentPackage)
      .subscribe(fpcPackage => this.packageUpdated.emit());
  }

  selectVersion(versionTag) {
    this.selectedVersion = null;
    this.packageVersionList.forEach((v, index) => {
      if (v.tag==versionTag) {
        if (index>0) {
          this.previousTag = this.packageVersionList[index-1].tag;
        } else {
          this.previousTag = null
        }
        this.selectedVersion = v;
        if (index<(this.packageVersionList.length-1)) {
          this.nextTag = this.packageVersionList[index+1].tag;
        } else {
          this.nextTag = null
        }

      }
    });
  }

  private updateRights() {
    this.mayEditPackage = (this.isAdmin || ((this.currentPackage) && this.currentPackage.ownerid==this.sub));
  }

  filterPackageVersionList() {
    this.packageVersionList = [];
    if ((this.currentPackage) && this.selectedFPCVersion) {
      this.packageVersionList = this.currentPackage.packageversionlist.filter(packageVersion => packageVersion.fpcversion===this.selectedFPCVersion.name);
      this.packageVersionList.sort((packageVersionA, packgageversionB) => VersionUtils.compare(packageVersionA.version, packgageversionB.version));
      const versionTag = this.route.snapshot.paramMap.get('version');
      if (this.packageVersionList.length > 0) {
        if (!versionTag) {
          this.selectVersion(this.packageVersionList[0].tag);
        } else {
          this.selectVersion(versionTag);
        }
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
    this.oidcSecurityService.getIsAuthorized().subscribe(
      (isAuthorized: boolean) => {
        this.categoryService.getCategoryList()
          .subscribe(categoryList => { this.categoryList = categoryList } )
      }
    );
    this.oidcSecurityService.getUserData().subscribe(
      (data: any) => {
        this.isAdmin = ((!!data) && (data.role == "admin"));
        if (data) {
          this.sub = data.sub;
        }
        this.updateRights();
      }
    );
    this.router.events.subscribe(
      (event: Event) => {
             if (event instanceof NavigationEnd) {
                  this.selectVersion(this.route.snapshot.paramMap.get('version'));
             }
      });
  }
}

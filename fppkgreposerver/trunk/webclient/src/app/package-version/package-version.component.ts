import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';
import { Router }    from '@angular/router';
import { FPCVersion } from '../fpcversion'
import { Package } from '../package'
import { Repository } from '../repository';
import { FppkgRepositoryService } from '../fppkg-repository.service';
import { PackageService } from '../package.service';
import { BuildManagerService } from '../build-manager.service';
import { RepPackage } from '../rep-package';
import { VersionUtils, Version } from '../version';
import { OidcSecurityService } from 'angular-auth-oidc-client';

@Component({
  selector: 'app-package-version',
  templateUrl: './package-version.component.html',
  styleUrls: ['./package-version.component.css']
})
export class PackageVersionComponent implements OnInit {

  public currentPackage: Package = null;

  @Input() packageVersion: any = null;
  @Input() packageVersionList: any[] = [];
  @Input() set package(selpackage: Package) {
    this.currentPackage = selpackage;
    if (!this.packageVersion.fpcversion) {
      this._packageService.getFPCVersionList().subscribe(versionlist => {
        for (var version of versionlist) {
          if (version.isdefault) {
            this.fpcversion = version;
            this._fppkgRepositoryService.getRepositoryList(version.name)
              .subscribe(repoList => this.initRepositoryList(repoList));
          }
        }
      })
    } else {
      this._packageService.getFPCVersionList().subscribe(versionlist => {
        for (var version of versionlist) {
          if (version.name == this.packageVersion.fpcversion) {
            this.fpcversion = version;
            this._fppkgRepositoryService.getRepositoryList(version.name)
              .subscribe(repoList => this.initRepositoryList(repoList));
          }
        }
      });
    }
  };
  @Input() mayEditPackage: boolean = false;
  @Output() packageUpdated = new EventEmitter();

  repositoryList : Repository[];
  fpcversion: FPCVersion;
  publishedVersionPerRepositoryMap: Object = {};
  isAdmin: Boolean;

  constructor(
    private _fppkgRepositoryService: FppkgRepositoryService,
    private _buildManagerService: BuildManagerService,
    private _packageService: PackageService,
    private _router: Router,
    private _oidcSecurityService: OidcSecurityService) { }

  ngOnInit() {
    this._oidcSecurityService.getUserData().subscribe(
      (data: any) => {
        this.isAdmin = ((!!data) && (data.role == "admin"));
      }
    );
  }

  initRepositoryList(repoList: Repository[]) {
    this.repositoryList = repoList;
    for (let repo of repoList) {
      this._fppkgRepositoryService.getRepPackageList(this.fpcversion.name, repo)
        .subscribe(repPackageList => {
          for (var repPackage of repPackageList) {
            if (repPackage.name == this.currentPackage.name) {

              let _version = this.packageVersionList.find(v => v.tag==repPackage.tag)
              let _obj = {
                tag: repPackage.tag,
                version: _version
              }
              this.publishedVersionPerRepositoryMap[repo.name] = _obj;

            }
          }
        });
    }
  }

  publishTag(repository: Repository,tag: string) {
    this._fppkgRepositoryService.addPackage(this.fpcversion.name, repository.name, this.currentPackage.name, tag)
      .subscribe(pkg => this.packageUpdated.emit());
  }

  updateTag(repository: Repository,tag: string) {
    this._fppkgRepositoryService.updatePackage(this.fpcversion.name, repository.name, this.currentPackage.name, tag)
      .subscribe(pkg => this.packageUpdated.emit());
  }

  requestBuild(tag) {
    this._buildManagerService.startBuildTask(this.currentPackage.name, tag, this.fpcversion.name)
      .subscribe(buildTask => this._router.navigate([`buildtask/${buildTask.uniquestring}`]))
  }

  compareVersion(val1: Version, val2: Version) {
    return VersionUtils.compare(val1, val2)
  }


}

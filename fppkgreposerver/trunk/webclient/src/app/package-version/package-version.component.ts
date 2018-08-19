import { Component, OnInit, Input } from '@angular/core';
import { Router }    from '@angular/router';
import { FPCVersion } from '../fpcversion'
import { Package } from '../package'
import { Repository } from '../repository';
import { FppkgRepositoryService } from '../fppkg-repository.service';
import { PackageService } from '../package.service';
import { BuildManagerService } from '../build-manager.service';
import { RepPackage } from '../rep-package';

@Component({
  selector: 'app-package-version',
  templateUrl: './package-version.component.html',
  styleUrls: ['./package-version.component.css']
})
export class PackageVersionComponent implements OnInit {

  @Input() packageVersion: any = null;
  @Input() package: Package = null;

  repositoryList : Repository[];
  fpcversion: FPCVersion;
  publishedVersionPerRepositoryMap: Object = {};

  constructor(
    private _fppkgRepositoryService: FppkgRepositoryService,
    private _buildManagerService: BuildManagerService,
    private _packageService: PackageService,
    private _router: Router) { }

  ngOnInit() {
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
  }

  initRepositoryList(repoList: Repository[]) {
    this.repositoryList = repoList;
    for (var repo of repoList) {
      this._fppkgRepositoryService.getRepPackageList(this.fpcversion.urlprefix , repo)
        .subscribe(repPackageList => {
          for (var repPackage of repPackageList) {
            if (repPackage.name == this.package.name) {
              this.publishedVersionPerRepositoryMap[repo.name] = repPackage.tag;
            }
          }
        });
    }
  }

  publishTag(repository: Repository,tag: string) {
    this._fppkgRepositoryService.addPackage(this.fpcversion.name, repository.name, this.package.name, tag)
      .subscribe(); //(pkg => this.packageUpdated.emit());
  }

  updateTag(repository: Repository,tag: string) {
    this._fppkgRepositoryService.updatePackage(this.fpcversion.name, repository.name, this.package.name, tag)
      .subscribe(); //(pkg => this.packageUpdated.emit());
  }

  requestBuild(tag) {
    this._buildManagerService.startBuildTask(this.package.name, tag)
      .subscribe(buildTask => this._router.navigate([`buildtask/${buildTask.uniquestring}`]))
  }

}

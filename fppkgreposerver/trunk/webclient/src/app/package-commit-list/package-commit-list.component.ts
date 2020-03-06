import { Component, OnInit, Input } from '@angular/core';
import { Package } from '../package'
import { RepositoryService } from '../repository.service';
import { PackageService } from '../package.service';
import { PackageRepoLog } from 'app/package-repo-log';
import { BuildManagerService } from '../build-manager.service';
import { Router }    from '@angular/router';
import { FPCVersion } from '../fpcversion'

@Component({
  selector: 'app-package-commit-list',
  templateUrl: './package-commit-list.component.html',
  styleUrls: ['./package-commit-list.component.css']
})
export class PackageCommitListComponent implements OnInit {

  @Input() packageVersion: any = null;
  @Input() package: Package = null;

  public isCollapsed = true;
  public packageLog : PackageRepoLog[];
  fpcversion: FPCVersion;

  constructor(
    private _repositoryService: RepositoryService,
    private _buildManagerService: BuildManagerService,
    private _packageService: PackageService,
    private _router: Router
  ) { }

  ngOnInit() {
    this._packageService.getFPCVersionList().subscribe(versionlist => {
      for (var version of versionlist) {
        if ((this.packageVersion.fpcversion && (version.name == this.packageVersion.fpcversion)) || ((!this.packageVersion.fpcversion) && version.isdefault)) {
          this.fpcversion = version;
          this._repositoryService.getPackageRepoLog(this.package.name, version)
            .subscribe(packageLog => this.packageLog = packageLog);
        }
      }
    })
  }

  requestBuild(tag) {
    this._buildManagerService.startBuildTask(this.package.name, tag, this.fpcversion.name)
      .subscribe(buildTask => this._router.navigate([`buildtask/${buildTask.uniquestring}`]))
  }
}

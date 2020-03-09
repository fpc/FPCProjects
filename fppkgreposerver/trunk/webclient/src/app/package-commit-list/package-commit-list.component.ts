import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';
import { Package } from '../package'
import { RepositoryService } from '../repository.service';
import { NgbModal } from '@ng-bootstrap/ng-bootstrap';
import { PackageRepoLog } from 'app/package-repo-log';
import { BuildManagerService } from '../build-manager.service';
import { TagPackageComponent } from '../tag-package/tag-package.component';
import { Router }    from '@angular/router';
import { FPCVersion } from '../fpcversion'
import { CurrentFpcversionService } from '../current-fpcversion.service';

@Component({
  selector: 'app-package-commit-list',
  templateUrl: './package-commit-list.component.html',
  styleUrls: ['./package-commit-list.component.css']
})
export class PackageCommitListComponent implements OnInit {

  highestTaggedVersion: string = '';

  public currentPackage: Package = null;
  @Input() set package(selpackage: Package) {
    this.currentPackage = selpackage;
    this.currentFpcversionService.getCurrentVersion()
      .subscribe(version => {
        this.fpcversion = version;
        this.packageLog = [];
        var tagNotSet = true;
        this._repositoryService.getPackageRepoLog(this.currentPackage.name, version)
        .subscribe(packageLog => this.packageLog = packageLog.map(logLine => {
          if ((logLine.tags) && (logLine.tags != 'initial')) {
            tagNotSet = false;
            if (!this.highestTaggedVersion) {
              this.highestTaggedVersion = logLine.version;
            }
          }
          logLine.allowTag = tagNotSet;
          return logLine;
        }));
      });
  };
  @Input() mayEditPackage: boolean = false;
  @Output() packageUpdated = new EventEmitter();

  public packageLog: PackageRepoLog[];
  fpcversion: FPCVersion;

  constructor(
    private _repositoryService: RepositoryService,
    private _buildManagerService: BuildManagerService,
    private _router: Router,
    private _modalService: NgbModal,
    private currentFpcversionService: CurrentFpcversionService
  ) { }

  ngOnInit() {
  }

  requestBuild(tag) {
    this._buildManagerService.startBuildTask(this.currentPackage.name, tag, this.fpcversion.name)
      .subscribe(buildTask => this._router.navigate([`buildtask/${buildTask.uniquestring}`]))
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
}

import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';
import { NgbModal } from '@ng-bootstrap/ng-bootstrap';
import { Router }    from '@angular/router';
import { UploadPackageComponent } from '../upload-package/upload-package.component';
import { TagPackageComponent } from '../tag-package/tag-package.component';
import { PackageService } from '../package.service';
import { BuildManagerService } from '../build-manager.service';
import { Package } from '../package';

@Component({
  selector: 'app-package',
  templateUrl: './package.component.html',
  styleUrls: ['./package.component.css']
})
export class PackageComponent implements OnInit {

  @Input() package: Package = null;
  @Output() packageUpdated = new EventEmitter();

  constructor(
    private _modalService: NgbModal,
    private _packageService: PackageService,
    private _buildManagerService: BuildManagerService,
    private _router: Router) { }

  showUploadSourceDialog() {
    const modalRef = this._modalService.open(UploadPackageComponent);
    modalRef.componentInstance.package = this.package;
    modalRef.componentInstance.packageSourceUploadedEvent.subscribe(event => this.packageUpdated.emit());

    this._packageService.getPackage(this.package.name)
      .subscribe(fpcPackage => this.package = fpcPackage);
  }

  showTagDialog() {
    const modalRef = this._modalService.open(TagPackageComponent);
    modalRef.componentInstance.package = this.package;

    modalRef.result.then(modalResult => {
      if (modalResult == 'tagged') {
        this.packageUpdated.emit();
      }
    }, err => {});
  }

  approvePackage() {
    this._packageService.approvePackage(this.package.name)
      .subscribe(fpcPackage => this.packageUpdated.emit());
  }

  requestBuild(tag) {
    this._buildManagerService.startBuildTask(this.package.name, tag)
      .subscribe(buildTask => this._router.navigate([`buildtask/${buildTask.uniquestring}`]))
  }

  ngOnInit() {
  }
}

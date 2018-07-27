import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';
import { NgbModal } from '@ng-bootstrap/ng-bootstrap';
import { UploadPackageComponent } from '../upload-package/upload-package.component';
import { TagPackageComponent } from '../tag-package/tag-package.component';
import { PackageService } from '../package.service';
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
    private _packageService: PackageService) { }

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

  ngOnInit() {
  }
}

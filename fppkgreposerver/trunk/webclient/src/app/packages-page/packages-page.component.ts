import { Component, OnInit } from '@angular/core';
import { NgbModal } from '@ng-bootstrap/ng-bootstrap';
import { AddPackageComponent } from '../add-package/add-package.component';
import { PackageService } from '../package.service';;
import { Package } from '../package';
import { FPCVersion } from '../fpcversion';

@Component({
  selector: 'app-packages-page',
  templateUrl: './packages-page.component.html',
  styleUrls: ['./packages-page.component.css']
})
export class PackagesPageComponent implements OnInit {

  packageList: Package[];
  fpcVersionList: FPCVersion[];
  selectedVersion: FPCVersion = null;

  constructor(
    private modalService: NgbModal,
    private packageService: PackageService) { }

  ngOnInit() {
    this.packageService.getPackageList()
      .subscribe(packageList => this.packageList = packageList )
    this.packageService.getFPCVersionList()
      .subscribe(list => {
        this.fpcVersionList = list;
        this.selectedVersion = list[0];
      });
  }

  open() {
    const modalRef = this.modalService.open(AddPackageComponent);
    modalRef.componentInstance.packageList = this.packageList;
  }

  onSelect(version: FPCVersion): void {
    this.selectedVersion = version;
  }

}

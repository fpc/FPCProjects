import { Component, OnInit } from '@angular/core';
import { NgbModal } from '@ng-bootstrap/ng-bootstrap';
import { AddPackageComponent } from '../add-package/add-package.component';
import { PackageService } from '../package.service';;
import { Package } from '../package';

@Component({
  selector: 'app-packages-page',
  templateUrl: './packages-page.component.html',
  styleUrls: ['./packages-page.component.css']
})
export class PackagesPageComponent implements OnInit {

  packageList: Package[];

  constructor(
    private modalService: NgbModal,
    private packageService: PackageService) { }

  ngOnInit() {
    this.packageService.getPackageList()
    .subscribe(packageList => this.packageList = packageList )

  }

  open() {
    const modalRef = this.modalService.open(AddPackageComponent);
    modalRef.componentInstance.packageList = this.packageList;
  }

}

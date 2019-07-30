import { Component, OnInit } from '@angular/core';
import { NgbModal } from '@ng-bootstrap/ng-bootstrap';
import { AddPackageComponent } from '../add-package/add-package.component';
import { PackageService } from '../package.service';
import { Package } from '../package';
import { FPCVersion } from '../fpcversion';

@Component({
  selector: 'app-packages-page',
  templateUrl: './packages-page.component.html',
  styleUrls: ['./packages-page.component.css']
})
export class PackagesPageComponent implements OnInit {

  packageList: Package[];
  filteredPackageList: Package[];
  selectedVersion: FPCVersion = null;

  constructor(
    private modalService: NgbModal,
    private packageService: PackageService) { }

  ngOnInit() {
    this.packageService.getPackageList()
      .subscribe(packageList => { this.packageList = packageList; this.filteredPackageList = packageList } )
  }

  open() {
    const modalRef = this.modalService.open(AddPackageComponent);
    modalRef.componentInstance.packageList = this.packageList;
  }

  setFPCVersion(version: FPCVersion): void {
    this.selectedVersion = version;
  }

  search(term: string): void {
    var prio1MatchPackageList: Package[] = [];
    var prio2MatchPackageList: Package[] = [];
    var prio3MatchPackageList: Package[] = [];

    term = term.toLocaleLowerCase();
    var termArray = term.split(' ');
    this.packageList.forEach(pck => {
      var match = true;
      var matchPoints = 0;
      termArray.forEach(trm => {
        if (pck.name.toLocaleLowerCase().indexOf(trm) !== -1) {
          matchPoints += 3;
        } else {
          match = false;
        }
      });

      if (match) {
        if (matchPoints >= (termArray.length * 3)) {
          prio1MatchPackageList.push(pck)
        } else if (matchPoints => (termArray.length * 2)) {
          prio2MatchPackageList.push(pck)
        } else {
          prio3MatchPackageList.push(pck)
        }
      }

      });
    this.filteredPackageList = prio1MatchPackageList;
    this.filteredPackageList.concat(prio2MatchPackageList);
    this.filteredPackageList.concat(prio3MatchPackageList);
  }
}

import { Component, OnInit, Output, EventEmitter } from '@angular/core';
import { FPCVersion } from '../fpcversion';
import { PackageService } from '../package.service';
import { CurrentFpcversionService } from '../current-fpcversion.service';

@Component({
  selector: 'app-fpc-version-select-box',
  templateUrl: './fpc-version-select-box.component.html',
  styleUrls: ['./fpc-version-select-box.component.css']
})
export class FpcVersionSelectBoxComponent implements OnInit {

  @Output() selectVersion = new EventEmitter<FPCVersion>();

  fpcVersionList: FPCVersion[];
  selectedVersion: FPCVersion = null;

  constructor(
    private packageService: PackageService,
    private currentFpcversionService: CurrentFpcversionService) { }

  ngOnInit() {
    this.packageService.getFPCVersionList()
      .subscribe(list => {
        this.fpcVersionList = list;
      });
    this.currentFpcversionService.getCurrentVersion()
      .subscribe(version => {
        this.selectedVersion = version;
      });
  }

  onSelect(version: FPCVersion): void {
    this.selectedVersion = version;
    this.currentFpcversionService.setCurrentVersion(version);
    this.selectVersion.emit(version);
  }
}

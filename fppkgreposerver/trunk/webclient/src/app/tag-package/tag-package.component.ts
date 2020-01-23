import { Component, OnInit, Input } from '@angular/core';
import { NgbActiveModal } from '@ng-bootstrap/ng-bootstrap';
import { Package } from '../package';
import { RepositoryService } from '../repository.service';
import { HttpErrorResponse } from '@angular/common/http';
import { FPCVersion } from '../fpcversion'
import { CurrentFpcversionService } from '../current-fpcversion.service';

@Component({
  selector: 'app-tag-package',
  templateUrl: './tag-package.component.html',
  styleUrls: ['./tag-package.component.css']
})
export class TagPackageComponent implements OnInit {

  tagMessage: string;
  isError: Boolean = false;
  isBusy: Boolean = false;
  errorMsg: string;
  selectedFPCVersion: FPCVersion = null;

  @Input() package: Package = null;

  constructor(
    public activeModal: NgbActiveModal,
    private currentFpcversionService: CurrentFpcversionService,
    private _repositoryService: RepositoryService,
  ) { }

  ngOnInit() {
    this.currentFpcversionService.getCurrentVersion()
      .subscribe(version => { this.selectedFPCVersion = version } );
  }

  tagPackage(fpcversion: FPCVersion) {
    if (this.tagMessage == '') {
      this.isError = true;
      this.errorMsg = 'You have to specify a tagmessage';
    }
    this.isError = false;
    this.isBusy = true;
    this._repositoryService.tagPackage(this.package.name, this.tagMessage, fpcversion)
      .subscribe(
        (response) => {
          this.isBusy = false;
          this.activeModal.close('tagged');
        },
        (err: HttpErrorResponse) => {
          this.isError = true;
          this.isBusy = false;
          this.errorMsg = 'Call to the repository service failed. ' + err.message;
        })
  }
}

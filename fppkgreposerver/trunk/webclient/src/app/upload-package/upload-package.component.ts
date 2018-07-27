import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';
import { RepositoryService } from '../repository.service';
import { Package } from '../package';
import { FPCVersion } from '../fpcversion';
import { HttpHeaders, HttpClient, HttpRequest, HttpEvent, HttpEventType, HttpErrorResponse } from '@angular/common/http';
import { PackageService } from '../package.service';
import { NgbActiveModal } from '@ng-bootstrap/ng-bootstrap';

export class LogMessage{
  timestamp: string;
  type: string;
  message: string;
}

@Component({
  selector: 'app-upload-package',
  templateUrl: './upload-package.component.html',
  styleUrls: ['./upload-package.component.css']
})
export class UploadPackageComponent implements OnInit {

  @Input() package: Package;
  @Output() packageSourceUploadedEvent = new EventEmitter<boolean>();

  fpcversionList : FPCVersion[];

  isError: boolean = false;
  isBusy: boolean = false;
  errorMsg: string = '';
  files: FileList = null;

  constructor(
    private repositoryService: RepositoryService,
    public activeModal: NgbActiveModal,
    private packageService: PackageService) {}

  onChange(files) {
    this.files = files;
  }

  uploadPackage(fpcversion: FPCVersion) {
    if ((this.files == null) || (this.files.length!=1)) {
      this.isError = true;
      this.isBusy = false;
      this.errorMsg = 'Please select a file to upload first';
      return
    }
    this.repositoryService.uploadPackage(this.files[0], this.package.name, fpcversion)
      .subscribe(
        (event: HttpEvent<any>) => {
          switch (event.type) {
            case HttpEventType.Sent:
              this.isBusy = true;
              break;
            case HttpEventType.ResponseHeader:
              break;
            case HttpEventType.DownloadProgress:
              break;
            case HttpEventType.UploadProgress:
              break;
            case HttpEventType.Response:
              this.isBusy = false;
              this.isError = false;
              this.activeModal.close('Source uploaded')
              this.packageSourceUploadedEvent.emit(true);
            }
        },
        err => {
          this.isError = true;
          this.isBusy = false;
          this.errorMsg = err.message;
        }
      );
  }

  ngOnInit() {
    this.packageService.getFPCVersionList()
      .subscribe(list => this.fpcversionList = list);
  }

}

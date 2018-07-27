import { Component, OnInit, Input } from '@angular/core';
import { BuildAgentService } from '../build-agent.service';
import { HttpHeaders, HttpClient, HttpRequest, HttpEvent, HttpEventType, HttpErrorResponse } from '@angular/common/http';
import { BuildAgent } from '../build-agent'

export class LogMessage{
  timestamp: string;
  type: string;
  message: string;
}

@Component({
  selector: 'app-packageupload',
  templateUrl: './packageupload.component.html',
  styleUrls: ['./packageupload.component.css']
})
export class PackageuploadComponent implements OnInit {
  isError: boolean = false;
  isBusy: boolean = false;
  errorMsg: string = '';
  buildAgentResponse: any;
  files: FileList = null;

  @Input() buildagent: BuildAgent = null;

  constructor(private buildAgentService: BuildAgentService) {}

  onChange(files) {
    this.files = files;
  }

  uploadPackage() {
    this.buildAgentResponse = null;
    if ((this.files == null) || (this.files.length!=1)) {
      this.isError = true;
      this.isBusy = false;
      this.errorMsg = 'Please select a file to upload first';
      return
    }
    this.buildAgentService.buildPackage(this.buildagent, this.files[0]).subscribe(
      (event: HttpEvent<any>) => {
        switch (event.type) {
          case HttpEventType.Sent:
            this.isBusy = true;
            break;
          case HttpEventType.ResponseHeader:
            console.log('Response header received!');
            break;
          case HttpEventType.DownloadProgress:
            const kbLoaded = Math.round(event.loaded / 1024);
            console.log(`Download in progress! ${ kbLoaded }Kb loaded`);
            break;
          case HttpEventType.UploadProgress:
            //const kbLoaded = Math.round(event.loaded / 1024);
            console.log(`Upload in progress!`);
            break;
          case HttpEventType.Response:
            this.isError = false;
            this.isBusy = false;
            this.buildAgentResponse = event.body;
        }
      },
      (err: HttpErrorResponse) => {
        this.isError = true;
        this.isBusy = false;
        this.errorMsg = 'Call to the Build-Agent failed. ' + err.message;
      }
    );

  }

  ngOnInit() {
  }
}

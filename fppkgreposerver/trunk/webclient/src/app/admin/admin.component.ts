import { Component, OnInit } from '@angular/core';
import { BuildAgentService } from '../build-agent.service';
import { HttpClient, HttpEvent, HttpEventType, HttpErrorResponse, HttpResponse } from '@angular/common/http';

@Component({
  selector: 'app-admin',
  templateUrl: './admin.component.html',
  styleUrls: ['./admin.component.css']
})
export class AdminComponent implements OnInit {

  constructor(private buildAgentService: BuildAgentService) { }

  isError: boolean = false;
  isBusy: boolean = false;
  errorMsg: string = '';
  buildAgentResponse: any;

  ngOnInit() {
  }

  rebuildTestEnvironment() {
    this.buildAgentResponse = null;
    this.buildAgentService.buildFPCEnvironment().subscribe(
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
              this.closeError();
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

  private closeError(){
    this.isError = false;
  }
}

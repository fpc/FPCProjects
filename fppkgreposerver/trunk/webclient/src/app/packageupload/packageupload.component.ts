import { Component, OnInit } from '@angular/core';
import { FileUploader } from 'ng2-file-upload';
import { FileItem } from 'ng2-file-upload';
import { ParsedResponseHeaders } from 'ng2-file-upload';

const URL = 'http://localhost:8080/build?cputarget=x86_64&ostarget=linux&fpcversion=3.1.1&loglevel=error,warning,info,debug';

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
  public uploader:FileUploader = new FileUploader({url: URL});
  uploadResponse: Array<LogMessage> = [];

  constructor() {
    this.uploader.options.disableMultipart = true;
    this.uploader.onProgressItem = this.doProgressItem;
    this.uploader.onCompleteItem = (item, response, status, headers) => this.doCompleteItem(item, response, status, headers);
  }

  doCompleteItem(item: FileItem, response: string, status: number, headers: ParsedResponseHeaders): any {
    this.uploadResponse = [];
    var responseArray = item._xhr.responseText.split('\n');
    responseArray.forEach(respString => {
      if (respString) {
        let respLogMessage = <LogMessage> JSON.parse(respString);
        this.uploadResponse.push(respLogMessage);
        }
    })
  }

  public doProgressItem(fileItem: FileItem, progress: any): any {
    //console.log('progress');
  }
  ngOnInit() {
  }
}

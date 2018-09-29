import { Component, OnInit } from '@angular/core';
import { BuildAgentService } from '../build-agent.service';
import { BuildManagerService } from '../build-manager.service';
import { HttpClient, HttpEvent, HttpEventType, HttpErrorResponse, HttpResponse } from '@angular/common/http';
import { FppkgRepositoryService } from '../fppkg-repository.service';
import { PackageService } from '../package.service';
import { Repository } from '../repository';
import { BuildAgent } from '../build-agent';
import { FPCVersion } from '../fpcversion';

class VersionRepoListCombined {
  fpcversion: FPCVersion;
  repoList: Repository[];
}

@Component({
  selector: 'app-admin',
  templateUrl: './admin.component.html',
  styleUrls: ['./admin.component.css']
})
export class AdminComponent implements OnInit {

  constructor(
    private _buildAgentService: BuildAgentService,
    private _buildManagerService: BuildManagerService,
    private _packageService: PackageService,
    private _fppkgRepositoryService: FppkgRepositoryService) { }

  isError: boolean = false;
  isBusy: boolean = false;
  errorMsg: string = '';
  buildAgentResponse: any;
  buildagentList: BuildAgent[];
  versionRepoList: VersionRepoListCombined[] = [];

  ngOnInit() {
    this._buildManagerService.getBuildAgentList()
      .subscribe(list => this.buildagentList = list);
    this._packageService.getFPCVersionList()
      .subscribe(list => {
        for (var version of list) {
          this.addVersion(version);
        }
      });
  }

  rebuildTestEnvironment(buildagent: BuildAgent) {
    this.buildAgentResponse = null;
    this._buildAgentService.buildFPCEnvironment(buildagent).subscribe(
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

  checkoutFPCCode(buildagent: BuildAgent) {
    this.buildAgentResponse = null;
    this._buildAgentService.checkoutFPCCode(buildagent).subscribe(
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


  private addVersion(version: FPCVersion) {
    this._fppkgRepositoryService.getRepositoryList(version.name)
      .subscribe(repList => {
        var combi: VersionRepoListCombined = {fpcversion: version, repoList: repList};
        this.versionRepoList.push(combi)
      });
  }

  private closeError(){
    this.isError = false;
  }
}

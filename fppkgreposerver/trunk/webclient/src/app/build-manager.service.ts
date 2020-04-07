import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders, HttpRequest } from '@angular/common/http';
import { OidcSecurityService } from 'angular-auth-oidc-client';
import { BuildTask } from './build-task';
import { BuildAgent } from './build-agent';
import { Observable } from 'rxjs';
import { AppConfigService } from './app-config.service';

@Injectable()
export class BuildManagerService {

    constructor(
      private http: HttpClient,
      private appConfigService: AppConfigService,
      private _securityService: OidcSecurityService) { }

    getHeaders(): HttpHeaders {
      let authheaders: HttpHeaders;
      let token = this._securityService.getToken();
      if (token !== '') {
        let tokenValue = 'Bearer ' + token;
        authheaders = new HttpHeaders({'authorization': tokenValue});
      } else {
        authheaders = new HttpHeaders();
      }
      return authheaders;
    }

    startBuildTask(aPackageName, aTag, aFPCVersion: string): Observable<BuildTask> {
      const url = `${this.appConfigService.BuildManagerUrl}/buildtask`;
      let newBuildTask: BuildTask = {
        packagename: aPackageName,
        tag: aTag,
        state: 'Unknown',
        fpcversion: aFPCVersion,
        uniquestring: null,
        subtasks: null
      }
      return this.http.post<BuildTask>(url, newBuildTask, {headers: this.getHeaders()});
    }

    getBuildTask(uniqueString: string): Observable<BuildTask> {
      const url = `${this.appConfigService.BuildManagerUrl}/buildtask/${uniqueString}`;
      return this.http.get<BuildTask>(url, {headers: this.getHeaders()});
    }

    getBuildAgentList(): Observable<BuildAgent[]> {
      const url = `${this.appConfigService.BuildManagerUrl}/agent/list`;
      return this.http.get<BuildAgent[]>(url, {headers: this.getHeaders()});
    }
}

import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders, HttpRequest } from '@angular/common/http';
import { OidcSecurityService } from './auth/services/oidc.security.service';
import { BuildTask } from './build-task';
import { BuildAgent } from './build-agent';
import { Observable } from 'rxjs/Observable';
import { environment } from '../environments/environment';

@Injectable()
export class BuildManagerService {

  private buildManagerURL = environment.buildManagerUrl;

    constructor(
      private http: HttpClient,
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

    startBuildTask(aPackageName, aTag: string): Observable<BuildTask> {
      const url = `${this.buildManagerURL}/buildtask`;
      let newBuildTask: BuildTask = {
        packagename: aPackageName,
        tag: aTag,
        state: 'Unknown',
        uniquestring: null,
        subtasks: null
      }
      return this.http.post<BuildTask>(url, newBuildTask, {headers: this.getHeaders()});
    }

    getBuildTask(uniqueString: string): Observable<BuildTask> {
      const url = `${this.buildManagerURL}/buildtask/${uniqueString}`;
      return this.http.get<BuildTask>(url, {headers: this.getHeaders()});
    }

    getBuildAgentList(): Observable<BuildAgent[]> {
      const url = `${this.buildManagerURL}/agent/list`;
      return this.http.get<BuildAgent[]>(url, {headers: this.getHeaders()});
    }
}

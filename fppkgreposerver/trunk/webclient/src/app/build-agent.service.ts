import { Injectable } from '@angular/core';
import { HttpHeaders, HttpClient, HttpRequest, HttpEvent, HttpEventType, HttpErrorResponse } from '@angular/common/http';
import { OidcSecurityService } from './auth/services/oidc.security.service';
import { PackageService } from './package.service';
import { Observable } from 'rxjs/Observable';
import { environment } from '../environments/environment';
import { BuildAgent } from './build-agent';
import { FPCVersion } from './fpcversion';

@Injectable()
export class BuildAgentService {

  private buildAgentUrl = environment.buildAgentUrl;

  constructor(
    private _http: HttpClient,
    private _packageService: PackageService,
    private _securityService: OidcSecurityService) { }

  private executeBuildAgentCommand(buildagent: BuildAgent, command: string): Observable<any> {
    let headers: HttpHeaders;
    let token = this._securityService.getToken();
    if (token !== '') {
      let tokenValue = 'Bearer ' + token;
      headers = new HttpHeaders({'authorization': tokenValue});
    } else {
      headers = new HttpHeaders();
    }

    const req = new HttpRequest('GET', buildagent.url+`${command}?cputarget=x86_64&ostarget=linux&fpcversion=${buildagent.fpcversion}&loglevel=error,warning,info,debug&chunked=false`, {
      requestProgress: true,
      headers: headers
    });
    return this._http.request(req);
  }

  buildFPCEnvironment(buildagent: BuildAgent): Observable<any> {
    return this.executeBuildAgentCommand(buildagent, 'buildfpcenvironment');
  }

  checkoutFPCCode(buildagent: BuildAgent): Observable<any> {
    return this.executeBuildAgentCommand(buildagent, 'checkoutfpccode');
  }

  buildPackage(buildagent: BuildAgent, file): Observable<any> {
    let headers: HttpHeaders;
    let token = this._securityService.getToken();
    if (token !== '') {
      let tokenValue = 'Bearer ' + token;
      headers = new HttpHeaders({'authorization': tokenValue});
    } else {
      headers = new HttpHeaders();
    }

    return this._packageService.getFPCVersionList().flatMap(versionlist => {
      const req = new HttpRequest('POST', buildagent.url+`build?cputarget=x86_64&ostarget=linux&fpcversion=${buildagent.fpcversion}&loglevel=error,warning,info,debug&chunked=false`, file, {
        headers: headers
      });
      return this._http.request(req);
    })
  }

  private handleError(error: any): Promise<any> {
    return Promise.reject(error.message || error);
  }
}

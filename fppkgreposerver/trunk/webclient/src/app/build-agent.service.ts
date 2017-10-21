import { Injectable } from '@angular/core';
import { HttpHeaders, HttpClient, HttpRequest, HttpEvent, HttpEventType, HttpErrorResponse } from '@angular/common/http';
import { OidcSecurityService } from './auth/services/oidc.security.service';
import { Observable } from 'rxjs/Observable';

@Injectable()
export class BuildAgentService {

  private buildAgentUrl = 'http://localhost:8080/';

  constructor(private _http: HttpClient, private _securityService: OidcSecurityService) { }

  setBuildAgentUrl(url: string) {
    this.buildAgentUrl = url;
  }

  buildFPCEnvironment(): Observable<any> {
    let headers: HttpHeaders;
    let token = this._securityService.getToken();
    if (token !== '') {
      let tokenValue = 'Bearer ' + token;
      headers = new HttpHeaders({'authorization': tokenValue});
    } else {
      headers = new HttpHeaders();
    }

    const req = new HttpRequest('GET', this.buildAgentUrl+'/buildfpcenvironment?cputarget=x86_64&ostarget=linux&fpcversion=3.1.1&loglevel=error,warning,info,debug&chunked=false', {
      requestProgress: true,
      //responseType: 'text',
      headers: headers
    });

    return this._http.request(req);
  }

  buildPackage(file): Observable<any> {
    let headers: HttpHeaders;
    let token = this._securityService.getToken();
    if (token !== '') {
      let tokenValue = 'Bearer ' + token;
      headers = new HttpHeaders({'authorization': tokenValue});
    } else {
      headers = new HttpHeaders();
    }
    const req = new HttpRequest('POST', this.buildAgentUrl+'/build?cputarget=x86_64&ostarget=linux&fpcversion=3.1.1&loglevel=error,warning,info,debug&chunked=false', file, {
      headers: headers
    });

    return this._http.request(req);
  }

  private handleError(error: any): Promise<any> {
    return Promise.reject(error.message || error);
  }
}

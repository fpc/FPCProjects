import { Injectable } from '@angular/core';
import { HttpHeaders, HttpClient, HttpRequest, HttpEvent, HttpEventType, HttpErrorResponse } from '@angular/common/http';
import { OidcSecurityService } from 'angular-auth-oidc-client';
import { Observable } from 'rxjs/Observable';
import { FPCVersion } from './fpcversion';
import { AppConfigService } from './app-config.service';

@Injectable()
export class RepositoryService {

  private repositoryUrl;

  constructor(private _http: HttpClient, private _securityService: OidcSecurityService, private _appConfigService: AppConfigService) {
    this.repositoryUrl = this._appConfigService.RepositoryUrl;
  }

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


  uploadPackage(file, packageName: string, fpcversion: FPCVersion): Observable<any> {
    let formData = new FormData;
    formData.append('upload', file);
    const req = new HttpRequest('POST', this.repositoryUrl + `/package/${packageName}/${fpcversion.name}`, formData, {
      headers: this.getHeaders()
    });

    return this._http.request(req);
  }

  tagPackage(packageName, tagMessage: string, fpcversion: FPCVersion): Observable<any> {

    const url = `${this.repositoryUrl}/package/${packageName}/tagpackage/${fpcversion.name}`;
    return this._http.get<any>(url, {headers: this.getHeaders(), params: {message: tagMessage}});
  }


}
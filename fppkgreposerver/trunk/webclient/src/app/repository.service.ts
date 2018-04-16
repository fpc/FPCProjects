import { Injectable } from '@angular/core';
import { HttpHeaders, HttpClient, HttpRequest, HttpEvent, HttpEventType, HttpErrorResponse } from '@angular/common/http';
import { OidcSecurityService } from './auth/services/oidc.security.service';
import { Observable } from 'rxjs/Observable';
import { environment } from '../environments/environment';

@Injectable()
export class RepositoryService {

  private repositoryUrl = environment.repositoryUrl;

  constructor(private _http: HttpClient, private _securityService: OidcSecurityService) { }

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


  uploadPackage(file, packageName: string): Observable<any> {
    let formData = new FormData;
    formData.append('upload', file);
    const req = new HttpRequest('POST', this.repositoryUrl + `/package/${packageName}`, formData, {
      headers: this.getHeaders()
    });

    return this._http.request(req);
  }

  tagPackage(packageName, tagMessage: string): Observable<any> {

    const url = `${this.repositoryUrl}/package/${packageName}/tagpackage`;
    return this._http.get<any>(url, {headers: this.getHeaders(), params: {message: tagMessage}});
  }


}
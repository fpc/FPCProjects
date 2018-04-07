import { Injectable } from '@angular/core';
import { HttpHeaders, HttpClient, HttpRequest, HttpEvent, HttpEventType, HttpErrorResponse } from '@angular/common/http';
import { OidcSecurityService } from './auth/services/oidc.security.service';
import { Observable } from 'rxjs/Observable';
import { Repository } from './repository';

@Injectable()
export class FppkgRepositoryService {

  private repositoryUrl = 'http://localhost:8282/';

  constructor(private _http: HttpClient, private _securityService: OidcSecurityService) { }

  private getHeaders(): HttpHeaders {

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

  getRepositoryList (fpcVersion: string): Observable<Repository[]> {
    const url = `${this.repositoryUrl}repository/${fpcVersion}`;
    return this._http.get<Repository[]>(url, {headers: this.getHeaders()});
  }

  rebuildRepository(fpcVersion, repositoryName: string): Observable<any> {
    const url = `${this.repositoryUrl}repository/${fpcVersion}/${repositoryName}/rebuild`;
    return this._http.get<any>(url, {headers: this.getHeaders()});
  }

  addPackage(fpcVersion, repositoryName, packageName, packageTag: string): Observable<any> {
    const url = `${this.repositoryUrl}package/${fpcVersion}/${repositoryName}`;
    return this._http.post<any>(url, {name: packageName, tag: packageTag}, {headers: this.getHeaders()});
  }
}

import { Injectable } from '@angular/core';
import { HttpHeaders, HttpClient, HttpRequest, HttpEvent, HttpEventType, HttpErrorResponse } from '@angular/common/http';
import { OidcSecurityService } from './auth/services/oidc.security.service';
import { Observable } from 'rxjs/Observable';
import { shareReplay } from 'rxjs/operators';
import { Repository } from './repository';
import { environment } from '../environments/environment';
import { RepPackage } from './rep-package';

@Injectable()
export class FppkgRepositoryService {

  private repositoryUrl = environment.fppkgRepositoryUrl;
  private _getRepositoryListMap: Map<string, Observable<Repository[]>>;

  constructor(private _http: HttpClient, private _securityService: OidcSecurityService) {
    this._getRepositoryListMap = new Map<string, Observable<Repository[]>>();
  }

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
    if (!this._getRepositoryListMap.has(fpcVersion)) {
      const url = `${this.repositoryUrl}/repository/${fpcVersion}`;
      this._getRepositoryListMap.set(fpcVersion, this._http.get<Repository[]>(url, {headers: this.getHeaders()}).pipe(shareReplay()));
    }
    return this._getRepositoryListMap.get(fpcVersion);
  }

  getRepPackageList(fpcVersion: string, repository: Repository): Observable<RepPackage[]> {
    const url = `${this.repositoryUrl}/repository/${fpcVersion}/${repository.name}`;
    return this._http.get<RepPackage[]>(url, {headers: this.getHeaders()});
  }

  rebuildRepository(fpcVersion, repositoryName: string): Observable<any> {
    const url = `${this.repositoryUrl}/repository/${fpcVersion}/${repositoryName}/rebuild`;
    return this._http.get<any>(url, {headers: this.getHeaders()});
  }

  addPackage(fpcVersion, repositoryName, packageName, packageTag: string): Observable<any> {
    const url = `${this.repositoryUrl}/package/${fpcVersion}/${repositoryName}`;
    return this._http.post<any>(url, {name: packageName, tag: packageTag}, {headers: this.getHeaders()});
  }

  updatePackage(fpcVersion, repositoryName, packageName, packageTag: string): Observable<any> {
    const url = `${this.repositoryUrl}/package/${fpcVersion}/${repositoryName}/${packageName}`;
    return this._http.put<any>(url, {name: packageName, tag: packageTag}, {headers: this.getHeaders()});
  }
}

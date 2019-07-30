import { Injectable } from '@angular/core';
import { Observable } from 'rxjs/Observable';
import { shareReplay } from 'rxjs/operators';
import { HttpClient, HttpHeaders, HttpRequest } from '@angular/common/http';
import { OidcSecurityService } from './auth/services/oidc.security.service';
import { Package } from './package';
import { FPCVersion } from './fpcversion';
import { environment } from '../environments/environment';

@Injectable()
export class PackageService {

  private packageManagerURL = environment.packageManagerUrl;
  private _getPackageList: Observable<Package[]>;
  private _getFPCVersionList: Observable<FPCVersion[]>;

  constructor(
    private http: HttpClient,
    private _securityService: OidcSecurityService) {
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

  getPackageList (): Observable<Package[]> {
    if (!this._getPackageList) {
      this._getPackageList = this.http.get<Package[]>(`${this.packageManagerURL}/package`, {headers: this.getHeaders()}).pipe(shareReplay());
    }
    return this._getPackageList;
  }

  getFPCVersionList (): Observable<FPCVersion[]> {
    if (!this._getFPCVersionList) {
      this._getFPCVersionList = this.http.get<FPCVersion[]>(`${this.packageManagerURL}/fpcversion`, {headers: this.getHeaders()}).pipe(shareReplay());
    }
    return this._getFPCVersionList;
  }

  getPackage(name: string): Observable<Package> {
    const url = `${this.packageManagerURL}/package/${name}`;
    return this.http.get<Package>(url, {headers: this.getHeaders()});
  }

  approvePackage(name: string): Observable<Package> {
    const url = `${this.packageManagerURL}/package/${name}/approve`;
    return this.http.put<Package>(url, null, {headers: this.getHeaders()});
  }

  addPackage(newPackage: Package): Observable<Package> {
    const url = `${this.packageManagerURL}/package`;
    return this.http.post<Package>(url, newPackage, {headers: this.getHeaders()});
  }



}

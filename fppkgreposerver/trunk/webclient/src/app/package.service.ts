import { Injectable } from '@angular/core';
import { Observable } from 'rxjs/Observable';
import { shareReplay } from 'rxjs/operators';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { OidcSecurityService } from 'angular-auth-oidc-client';
import { Package } from './package';
import { FPCVersion } from './fpcversion';
import { AppConfigService } from './app-config.service';

@Injectable()
export class PackageService {

  private packageManagerURL: string;
  private _getPackageList: Observable<Package[]>;
  private _getFPCVersionList: Observable<FPCVersion[]>;

  constructor(
    private http: HttpClient,
    private appConfigService: AppConfigService,
    private _securityService: OidcSecurityService) {
      this.packageManagerURL = this.appConfigService.PackageManagerUrl;
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

  patchPackageCategory(anPackage: Package): Observable<Package> {
    const url = `${this.packageManagerURL}/package/${anPackage.name}`;
    return this.http.patch<Package>(url, {categoryid: anPackage.categoryid}, {headers: this.getHeaders()});
  }

  patchSupportCategory(anPackage: Package): Observable<Package> {
    const url = `${this.packageManagerURL}/package/${anPackage.name}`;
    return this.http.patch<Package>(url, {support: anPackage.support}, {headers: this.getHeaders()});
  }
}

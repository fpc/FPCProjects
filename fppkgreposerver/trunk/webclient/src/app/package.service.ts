import { Injectable } from '@angular/core';
import { Observable } from 'rxjs/Observable';
import { map, shareReplay, switchMap, tap } from 'rxjs/operators';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { OidcSecurityService } from 'angular-auth-oidc-client';
import { BehaviorSubject } from 'rxjs/BehaviorSubject';
import { Package } from './package';
import { FPCVersion } from './fpcversion';
import { AppConfigService } from './app-config.service';

@Injectable()
export class PackageService {

  private packageManagerURL: string;
  private _packageList: Observable<Package[]>;
  private _reload = new BehaviorSubject<void>(null);
  private _getFPCVersionList: Observable<FPCVersion[]>;

  constructor(
    private http: HttpClient,
    private appConfigService: AppConfigService,
    private _securityService: OidcSecurityService) {
      this.packageManagerURL = this.appConfigService.PackageManagerUrl;
      this._securityService.getIsAuthorized().subscribe(isAuthorized => {
        this._reload.next(null)
      });
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

  private requestPackageList() {
    return this.http.get<Package[]>(`${this.packageManagerURL}/package`, {headers: this.getHeaders()}).pipe(
      map(response => response)
    );
  }

  public getPackageList (): Observable<Package[]> {
    // Extensive explanation can be found here:
    // https://blog.thoughtram.io/angular/2018/03/05/advanced-caching-with-rxjs.html
    if (!this._packageList) {
      this._packageList = this._reload.pipe(
        switchMap(_ => this.requestPackageList()),
        shareReplay(1)
      )
    }
    return this._packageList;
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

  removePackage(name: string): Observable<any> {
    const url = `${this.packageManagerURL}/package/${name}`;
    return this.http.delete<Package>(url, {headers: this.getHeaders()}).pipe(
      tap(_ => this._reload.next(null))
    );
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

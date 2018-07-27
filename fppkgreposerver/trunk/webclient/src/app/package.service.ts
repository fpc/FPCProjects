import { Injectable } from '@angular/core';
import { Observable } from 'rxjs/Observable';
import { HttpClient, HttpHeaders, HttpRequest } from '@angular/common/http';
import { OidcSecurityService } from './auth/services/oidc.security.service';
import { Package } from './package';
import { FPCVersion } from './fpcversion';
import { environment } from '../environments/environment';

@Injectable()
export class PackageService {

  private packageManagerURL = environment.packageManagerUrl;

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

  getPackageList (): Observable<Package[]> {
    const url = `${this.packageManagerURL}/package`;
    return this.http.get<Package[]>(url, {headers: this.getHeaders()});
  }

  getFPCVersionList (): Observable<FPCVersion[]> {
    const url = `${this.packageManagerURL}/fpcversion`;
    return this.http.get<FPCVersion[]>(url, {headers: this.getHeaders()});
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

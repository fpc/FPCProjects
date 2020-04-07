import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { OidcConfigService } from 'angular-auth-oidc-client';
import { forkJoin } from 'rxjs';

@Injectable()
export class AppConfigService {

  private appConfig: any;

  constructor(
    private http: HttpClient,
    private oidcConfigService: OidcConfigService
  ) { }

  loadAppConfig() {
    return forkJoin(
      this.http.get('assets/config.json'),
      this.oidcConfigService.load('assets/auth.clientConfiguration.json')
    )
    .toPromise()
    .then(data => {
        this.appConfig = data[0];
    })
  }

  private checkAppConfig(){
    if (!this.appConfig) {
      throw Error('Config file not loaded!');
    }
  }

  get IdentityServerUrl() {
    this.checkAppConfig();
    return this.appConfig.identityServerUrl;
  }

  get CategoryUrl() {
    this.checkAppConfig();
    return this.appConfig.categoryUrl;
  }

  get BuildManagerUrl() {
    this.checkAppConfig();
    return this.appConfig.buildManagerUrl;
  }

  get RepositoryUrl() {
    this.checkAppConfig();
    return this.appConfig.repositoryUrl;
  }

  get FppkgRepositoryUrl() {
    this.checkAppConfig();
    return this.appConfig.fppkgRepositoryUrl;
  }

  get PackageManagerUrl() {
    this.checkAppConfig();
    return this.appConfig.packageManagerUrl;
  }
}

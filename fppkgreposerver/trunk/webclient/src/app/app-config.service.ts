import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { AuthModule, OidcConfigService, OidcSecurityService } from 'angular-auth-oidc-client';
import { Observable } from 'rxjs/Rx';
import { environment } from '../environments/environment';


@Injectable()
export class AppConfigService {

  private appConfig: any;

  constructor(
    private http: HttpClient,
    private oidcConfigService: OidcConfigService
  ) { }

  loadAppConfig() {
    return Observable.forkJoin(
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

}

import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';

import { provideRoutes } from '@angular/router';
import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { HttpModule } from '@angular/http';
import { HttpClientModule } from '@angular/common/http';
import { FileUploadModule } from 'ng2-file-upload';
import { Angular2FontawesomeModule } from 'angular2-fontawesome/angular2-fontawesome'
import { FormsModule } from '@angular/forms';

import { AuthModule } from './auth/modules/auth.module';
import { AuthGuardService } from './auth-guard.service';
import { AdminGuardService } from './admin-guard.service';
import { OidcSecurityService } from './auth/services/oidc.security.service';
import { OpenIDImplicitFlowConfiguration } from './auth/modules/auth.configuration';
import { PackageuploadComponent } from './packageupload/packageupload.component';
import { NgbModule } from '@ng-bootstrap/ng-bootstrap';
import { NavbarComponent } from './navbar/navbar.component';
import { LoginComponent } from './login/login.component';
import { LoggedOutComponent } from './logged-out/logged-out.component';
import { BuildPackageComponent } from './build-package/build-package.component';
import { AdminComponent } from './admin/admin.component';
import { BuildAgentService } from './build-agent.service';
import { SpinnerComponent } from './spinner/spinner.component';
import { ViewDCSResponseComponent } from './view-dcsresponse/view-dcsresponse.component';
import { environment } from '../environments/environment';

@NgModule({
  declarations: [
    AppComponent,
    PackageuploadComponent,
    NavbarComponent,
    LoginComponent,
    LoggedOutComponent,
    BuildPackageComponent,
    AdminComponent,
    SpinnerComponent,
    ViewDCSResponseComponent
  ],
  imports: [
    BrowserModule,
    HttpModule,
    HttpClientModule,
    AppRoutingModule,
    FileUploadModule,
    Angular2FontawesomeModule,
    FormsModule,
    AuthModule.forRoot(),
    NgbModule.forRoot()
  ],
  providers: [
    OidcSecurityService,
    AuthGuardService,
    AdminGuardService,
    BuildAgentService
  ],
  bootstrap: [AppComponent]
})
export class AppModule {
  constructor(public oidcSecurityService: OidcSecurityService, public buildAgentService: BuildAgentService) {

    let openIDImplicitFlowConfiguration = new OpenIDImplicitFlowConfiguration();
    openIDImplicitFlowConfiguration.stsServer = environment.identityServerUrl;

    openIDImplicitFlowConfiguration.redirect_url = environment.webclientUrl;
    // The Client MUST validate that the aud (audience) Claim contains its client_id value registered at the Issuer identified by the iss (issuer) Claim as an audience.
    // The ID Token MUST be rejected if the ID Token does not list the Client as a valid audience, or if it contains additional audiences not trusted by the Client.
    openIDImplicitFlowConfiguration.client_id = 'FPPKGWebClient';
    openIDImplicitFlowConfiguration.response_type = 'id_token token';
    openIDImplicitFlowConfiguration.scope = 'openid profile role buildagent';
    openIDImplicitFlowConfiguration.post_logout_redirect_uri = environment.webclientUrl + '/unauthorized';
    openIDImplicitFlowConfiguration.start_checksession = true;
    openIDImplicitFlowConfiguration.silent_renew = true;
    openIDImplicitFlowConfiguration.startup_route = '/';
    // HTTP 403
    openIDImplicitFlowConfiguration.forbidden_route = '/forbidden';
    // HTTP 401
    openIDImplicitFlowConfiguration.unauthorized_route = '/unauthorized';
    openIDImplicitFlowConfiguration.log_console_warning_active = true;
    openIDImplicitFlowConfiguration.log_console_debug_active = false;
    // id_token C8: The iat Claim can be used to reject tokens that were issued too far away from the current time,
    // limiting the amount of time that nonces need to be stored to prevent attacks.The acceptable range is Client specific.
    openIDImplicitFlowConfiguration.max_id_token_iat_offset_allowed_in_seconds = 10;

    this.oidcSecurityService.setupModule(openIDImplicitFlowConfiguration);
    this.buildAgentService.setBuildAgentUrl(environment.buildAgentUrl);
  }
}

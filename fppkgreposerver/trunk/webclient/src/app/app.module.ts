import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';

import { provideRoutes } from '@angular/router';
import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { HttpModule } from '@angular/http';
import { FileUploadModule } from 'ng2-file-upload';
import { AlertModule } from 'ngx-bootstrap';

import { AuthModule } from './auth/modules/auth.module';
import { OidcSecurityService } from './auth/services/oidc.security.service';
import { OpenIDImplicitFlowConfiguration } from './auth/modules/auth.configuration';
import { PackageuploadComponent } from './packageupload/packageupload.component';

@NgModule({
  declarations: [
    AppComponent,
    PackageuploadComponent
  ],
  imports: [
    BrowserModule,
    HttpModule,
    AppRoutingModule,
    FileUploadModule,
    AlertModule.forRoot(),
    AuthModule.forRoot()
  ],
  providers: [OidcSecurityService],
  bootstrap: [AppComponent]
})
export class AppModule {
  constructor(public oidcSecurityService: OidcSecurityService) {

    let openIDImplicitFlowConfiguration = new OpenIDImplicitFlowConfiguration();
    openIDImplicitFlowConfiguration.stsServer = 'http://localhost:5000';

    openIDImplicitFlowConfiguration.redirect_url = 'http://localhost:4200';
    // The Client MUST validate that the aud (audience) Claim contains its client_id value registered at the Issuer identified by the iss (issuer) Claim as an audience.
    // The ID Token MUST be rejected if the ID Token does not list the Client as a valid audience, or if it contains additional audiences not trusted by the Client.
    openIDImplicitFlowConfiguration.client_id = 'FPPKGWebClient';
    openIDImplicitFlowConfiguration.response_type = 'id_token token';
    openIDImplicitFlowConfiguration.scope = 'openid';
    openIDImplicitFlowConfiguration.post_logout_redirect_uri = 'http://localhost:4200/unauthorized';
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
  }
}

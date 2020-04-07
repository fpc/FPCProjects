import { BrowserModule } from '@angular/platform-browser';
import { NgModule, APP_INITIALIZER } from '@angular/core';

import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { HttpModule } from '@angular/http';
import { HttpClientModule } from '@angular/common/http';
import { FormsModule } from '@angular/forms';
import { HTTP_INTERCEPTORS } from '@angular/common/http';

import { AuthModule, OidcConfigService, OidcSecurityService, OpenIDImplicitFlowConfiguration, AuthWellKnownEndpoints } from 'angular-auth-oidc-client';
import { AuthGuardService } from './auth-guard.service';
import { PackageService } from './package.service';
import { AdminGuardService } from './admin-guard.service';
import { PackageuploadComponent } from './packageupload/packageupload.component';
import { NgbModule } from '@ng-bootstrap/ng-bootstrap';
import { NavbarComponent } from './navbar/navbar.component';
import { LoginComponent } from './login/login.component';
import { LoggedOutComponent } from './logged-out/logged-out.component';
import { BuildPackageComponent } from './build-package/build-package.component';
import { AdminComponent } from './admin/admin.component';
import { BuildAgentService } from './build-agent.service';
import { RepositoryService } from './repository.service';
import { FppkgRepositoryService } from './fppkg-repository.service';
import { BuildManagerService } from './build-manager.service';
import { SpinnerComponent } from './spinner/spinner.component';
import { ViewDCSResponseComponent } from './view-dcsresponse/view-dcsresponse.component';
import { AboutComponent } from './about/about.component';
import { PackageListComponent } from './package-list/package-list.component';
import { PackageComponent } from './package/package.component';
import { PackagePageComponent } from './package-page/package-page.component';
import { PackagesPageComponent } from './packages-page/packages-page.component';
import { AddPackageComponent } from './add-package/add-package.component';
import { NgbActiveModal } from '@ng-bootstrap/ng-bootstrap';
import { HttpErrorInterceptor } from './http-error-interceptor';
import { UploadPackageComponent } from './upload-package/upload-package.component';
import { BuildTaskPageComponent } from './build-task-page/build-task-page.component';
import { BuildTaskComponent } from './build-task/build-task.component';
import { TagPackageComponent } from './tag-package/tag-package.component';
import { RebuildRepositoryComponent } from './rebuild-repository/rebuild-repository.component';
import { PackageVersionComponent } from './package-version/package-version.component';
import { PackageCardComponent } from './package-card/package-card.component';
import { FpcVersionSelectBoxComponent } from './fpc-version-select-box/fpc-version-select-box.component';
import { VersionPipe } from './version.pipe';
import { CurrentFpcversionService } from './current-fpcversion.service';
import { CategoryService } from './category.service';
import { CategoriesComponent } from './categories/categories.component';
import { ViewModeDirective } from './edit-in-place/view-mode.directive';
import { EditModeDirective } from './edit-in-place/edit-mode.directive';
import { EditableComponent } from './edit-in-place/editable/editable.component';
import { EditableOnEnterDirective } from './edit-in-place/editable-on-enter.directive';
import { EditableOnStartDirective } from './edit-in-place/editable-on-start.directive';
import { AppConfigService } from './app-config.service';
import { PackageCommitListComponent } from './package-commit-list/package-commit-list.component';
import { MarkdownModule } from 'ngx-markdown';
import { DocumentationComponent } from './documentation/documentation.component';

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
    ViewDCSResponseComponent,
    AboutComponent,
    PackageListComponent,
    PackageComponent,
    PackagePageComponent,
    PackagesPageComponent,
    AddPackageComponent,
    UploadPackageComponent,
    BuildTaskPageComponent,
    BuildTaskComponent,
    TagPackageComponent,
    RebuildRepositoryComponent,
    PackageVersionComponent,
    PackageCardComponent,
    FpcVersionSelectBoxComponent,
    VersionPipe,
    CategoriesComponent,
    ViewModeDirective,
    EditModeDirective,
    EditableComponent,
    EditableOnEnterDirective,
    EditableOnStartDirective,
    PackageCommitListComponent,
    DocumentationComponent
  ],
  imports: [
    BrowserModule,
    HttpModule,
    HttpClientModule,
    FormsModule,
    AuthModule.forRoot(),
    NgbModule.forRoot(),
    AppRoutingModule,
    MarkdownModule.forRoot()
  ],
  providers: [
    OidcSecurityService,
    AuthGuardService,
    AdminGuardService,
    BuildAgentService,
    BuildManagerService,
    PackageService,
    CategoryService,
    RepositoryService,
    FppkgRepositoryService,
    CurrentFpcversionService,
    AppConfigService,
    NgbActiveModal,
    {
      provide: HTTP_INTERCEPTORS,
      useClass: HttpErrorInterceptor,
      multi: true,
    },
    {
      provide: APP_INITIALIZER,
      multi: true,
      deps: [AppConfigService],
      useFactory: (appConfigService: AppConfigService) => {
        return () => {
          return appConfigService.loadAppConfig();
        };
      }
    }
  ],
  entryComponents: [
    AddPackageComponent,
    UploadPackageComponent,
    TagPackageComponent
  ],
  bootstrap: [AppComponent]
})
export class AppModule {
  constructor(
    private oidcSecurityService: OidcSecurityService,
    private oidcConfigService: OidcConfigService
  ) {
    this.oidcConfigService.onConfigurationLoaded.subscribe(() => {
      const openIDImplicitFlowConfiguration = new OpenIDImplicitFlowConfiguration();
      openIDImplicitFlowConfiguration.stsServer = this.oidcConfigService.clientConfiguration.stsServer;
      openIDImplicitFlowConfiguration.redirect_url = this.oidcConfigService.clientConfiguration.redirect_url;
      openIDImplicitFlowConfiguration.client_id = this.oidcConfigService.clientConfiguration.client_id;
      openIDImplicitFlowConfiguration.response_type = this.oidcConfigService.clientConfiguration.response_type;
      openIDImplicitFlowConfiguration.post_login_route = this.oidcConfigService.clientConfiguration.post_login_route;
      openIDImplicitFlowConfiguration.post_logout_redirect_uri = this.oidcConfigService.clientConfiguration.post_logout_redirect_uri;

      openIDImplicitFlowConfiguration.max_id_token_iat_offset_allowed_in_seconds = this.oidcConfigService.clientConfiguration.max_id_token_iat_offset_allowed_in_seconds;

      openIDImplicitFlowConfiguration.log_console_debug_active = this.oidcConfigService.clientConfiguration.log_console_debug_active;

      const authWellKnownEndpoints = new AuthWellKnownEndpoints();
      authWellKnownEndpoints.setWellKnownEndpoints(this.oidcConfigService.wellKnownEndpoints);

      this.oidcSecurityService.setupModule(openIDImplicitFlowConfiguration, authWellKnownEndpoints);
    });
  }
}

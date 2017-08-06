import { Component, OnInit, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs/Subscription';

import { OidcSecurityService } from './auth/services/oidc.security.service';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent implements OnInit, OnDestroy {
  title = 'The FPPKG Repository';

  isAuthorizedSubscription: Subscription;
  isAuthorized: boolean;

  constructor(public oidcSecurityService: OidcSecurityService) {
  }

  ngOnInit() {
      this.isAuthorizedSubscription = this.oidcSecurityService.getIsAuthorized().subscribe(
          (isAuthorized: boolean) => {
              this.isAuthorized = isAuthorized;
          });

      if (window.location.hash) {
          this.oidcSecurityService.authorizedCallback();
      }
  }

  ngOnDestroy(): void {
      this.isAuthorizedSubscription.unsubscribe();
  }

  login() {
      console.log('start login');
      this.oidcSecurityService.authorize();
  }

  refreshSession() {
      console.log('start refreshSession');
      this.oidcSecurityService.authorize();
  }

  logout() {
      console.log('start logoff');
      this.oidcSecurityService.logoff();
  }

  buildfpcenvironment() {

  }
}

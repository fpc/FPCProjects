import { Injectable } from '@angular/core';
import { CanActivate, Router }    from '@angular/router';
import { Subscription } from 'rxjs';
import { OidcSecurityService } from 'angular-auth-oidc-client';

@Injectable()
export class AuthGuardService implements CanActivate{

  isAuthorizedSubscription: Subscription;
  isAuthorized: boolean;

  constructor(private oidcSecurityService: OidcSecurityService, private router: Router) {
    this.isAuthorizedSubscription = this.oidcSecurityService.getIsAuthorized().subscribe(
      (isAuthorized: boolean) => {
          this.isAuthorized = isAuthorized;
      });
  }

  canActivate() {
    if (!this.isAuthorized) {
      this.router.navigate(['/unauthorized']);
    }
    return this.isAuthorized;
  }

}

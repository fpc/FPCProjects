import { Injectable } from '@angular/core';
import { CanActivate, Router }    from '@angular/router';
import { Subscription } from 'rxjs';
import { OidcSecurityService } from 'angular-auth-oidc-client';

@Injectable()
export class AdminGuardService {

  isAuthorizedSubscription: Subscription;
  isAuthorized: boolean;

  userDataSubscription: Subscription;
  isAdmin: boolean;

  constructor(private oidcSecurityService: OidcSecurityService, private router: Router) {
    this.isAuthorizedSubscription = this.oidcSecurityService.getIsAuthorized().subscribe(
      (isAuthorized: boolean) => {
          this.isAuthorized = isAuthorized;
      });
    this.userDataSubscription = this.oidcSecurityService.getUserData().subscribe(
      (data: any) => {
        this.isAdmin = ((!!data) && (data.role == "admin"));
      });
  }

  canActivate() {
    var canContinue: boolean = ((this.isAuthorized) && (this.isAdmin));
    if (!canContinue)  {
      this.router.navigate(['/forbidden']);
    }
    return canContinue;
  }
}

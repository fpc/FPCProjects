import { Injectable } from '@angular/core';
import { CanActivate, Router }    from '@angular/router';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { OidcSecurityService } from 'angular-auth-oidc-client';

@Injectable()
export class AuthGuardService implements CanActivate{

  constructor(private oidcSecurityService: OidcSecurityService, private router: Router) {
  }

  canActivate() {
    return this.checkUser();
  }

  private checkUser(): Observable<boolean> {
    return this.oidcSecurityService.getIsAuthorized().pipe(
        map((isAuthorized: boolean) => {
            if (!isAuthorized) {
                this.router.navigate(['/unauthorized']);
                return false;
            }
            return true;
        })
    );
  }
}

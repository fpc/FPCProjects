import { Component, OnInit } from '@angular/core';
import { Subscription } from 'rxjs/Subscription';
import { OidcSecurityService } from '../auth/services/oidc.security.service';

@Component({
  selector: 'app-navbar',
  templateUrl: './navbar.component.html',
  styleUrls: ['./navbar.component.css']
})
export class NavbarComponent implements OnInit {

  isAuthorizedSubscription: Subscription;
  isAuthorized: boolean;

  userDataSubscription: Subscription;
  isAdmin: boolean;

  constructor(public oidcSecurityService: OidcSecurityService) { }

  ngOnInit() {
    this.isAuthorizedSubscription = this.oidcSecurityService.getIsAuthorized().subscribe(
      (isAuthorized: boolean) => {
          this.isAuthorized = isAuthorized;
      });
      this.userDataSubscription = this.oidcSecurityService.getUserData().subscribe(
        (data: any) => {
          this.isAdmin = (data.role == "admin");
        }
      );
    }

  login() {
    this.oidcSecurityService.authorize();
  }
  logout() {
    this.oidcSecurityService.logoff();
  }

}

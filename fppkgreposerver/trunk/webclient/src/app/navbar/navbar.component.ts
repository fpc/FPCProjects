import { Component, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { OidcSecurityService } from 'angular-auth-oidc-client';

@Component({
  selector: 'app-navbar',
  templateUrl: './navbar.component.html',
  styleUrls: ['./navbar.component.css']
})
export class NavbarComponent implements OnInit {

  isAuthorized: boolean;

  userDataSubscription: Subscription;
  isAdmin: boolean;
  isNavbarCollapsed: boolean = false;

  constructor(public oidcSecurityService: OidcSecurityService) { }

  ngOnInit() {
    this.oidcSecurityService.getIsAuthorized().subscribe(
      (isAuthorized: boolean) => {
          this.isAuthorized = isAuthorized;
      });
      this.userDataSubscription = this.oidcSecurityService.getUserData().subscribe(
        (data: any) => {
          this.isAdmin = ((!!data) && (data.role == "admin"));
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

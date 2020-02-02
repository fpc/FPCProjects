import { Component, OnInit, Input, SimpleChanges } from '@angular/core';
import { Package } from '../package';
import { OidcSecurityService } from 'angular-auth-oidc-client';

@Component({
  selector: 'app-package-card',
  templateUrl: './package-card.component.html',
  styleUrls: ['./package-card.component.css']
})
export class PackageCardComponent implements OnInit {

  @Input() package: Package;
  @Input() fpcversion: any;
  selectedVersion: any = null;
  isAuthorized: boolean;

  constructor(public oidcSecurityService: OidcSecurityService) { }

  ngOnInit() {
    this.oidcSecurityService.getIsAuthorized().subscribe(
      (isAuthorized: boolean) => {
          this.isAuthorized = isAuthorized;
      }
    );
  }

  ngOnChanges(changes: SimpleChanges) {
    this.selectedVersion = null
    if (this.fpcversion && (this.package)) {
      for (var packageversion of this.package.packageversionlist) {
        if (packageversion.fpcversion == this.fpcversion.name) {
          this.selectedVersion = packageversion
        }
      }
    }
  }
}

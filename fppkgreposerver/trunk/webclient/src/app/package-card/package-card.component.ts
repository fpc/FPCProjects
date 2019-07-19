import { Component, OnInit, Input, SimpleChanges } from '@angular/core';
import { Package } from '../package';

@Component({
  selector: 'app-package-card',
  templateUrl: './package-card.component.html',
  styleUrls: ['./package-card.component.css']
})
export class PackageCardComponent implements OnInit {

  @Input() package: any;
  @Input() fpcversion: any;
  selectedVersion: any = null;

  constructor() { }

  ngOnInit() {
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

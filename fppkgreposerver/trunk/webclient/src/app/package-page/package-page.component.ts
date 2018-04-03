import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { PackageService } from '../package.service';
import { Package } from '../package';

@Component({
  selector: 'app-package-page',
  templateUrl: './package-page.component.html',
  styleUrls: ['./package-page.component.css']
})
export class PackagePageComponent implements OnInit {

  package: Package;

  constructor(
    private route: ActivatedRoute,
    private packageService: PackageService
  ) { }

  ngOnInit() {
    this.getPackage();
  }

  getPackage(): void {
    const name = this.route.snapshot.paramMap.get('name');
    this.packageService.getPackage(name)
      .subscribe(fpcPackage => this.package = fpcPackage);
  }
}

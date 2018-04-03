import { Component, OnInit, Input } from '@angular/core';
import { Package } from '../package';
import { PackageService } from '../package.service';

@Component({
  selector: 'app-package-list',
  templateUrl: './package-list.component.html',
  styleUrls: ['./package-list.component.css']
})
export class PackageListComponent implements OnInit {

  @Input() packageList: Package[];

  constructor(private packageService: PackageService) { }

  ngOnInit() {
  }

}

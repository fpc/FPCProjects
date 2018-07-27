import { Component, OnInit } from '@angular/core';
import { BuildManagerService } from '../build-manager.service';
import { BuildAgent } from '../build-agent';

@Component({
  selector: 'app-build-package',
  templateUrl: './build-package.component.html',
  styleUrls: ['./build-package.component.css']
})
export class BuildPackageComponent implements OnInit {

  buildagentList : BuildAgent[];

  constructor(
    private _buildManagerService: BuildManagerService) { }

  ngOnInit() {
    this._buildManagerService.getBuildAgentList()
      .subscribe(list => this.buildagentList = list);
}

}

import { Component, OnInit } from '@angular/core';
import { BuildTask } from '../build-task'
import { BuildManagerService } from '../build-manager.service'
import { ActivatedRoute } from '@angular/router';

@Component({
  selector: 'app-build-task-page',
  templateUrl: './build-task-page.component.html',
  styleUrls: ['./build-task-page.component.css']
})
export class BuildTaskPageComponent implements OnInit {

  buildTask: BuildTask;

  constructor(
    private buildManagerService: BuildManagerService,
    private route: ActivatedRoute
  ) { }

  ngOnInit() {
    this.getBuildTask();
  }

  getBuildTask(): void {
    const uniqueString = this.route.snapshot.paramMap.get('uniqueString');
    this.buildManagerService.getBuildTask(uniqueString)
      .subscribe(buildTask => this.buildTask = buildTask);
  }

}

import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';
import { BuildTask } from '../build-task'

@Component({
  selector: 'app-build-task',
  templateUrl: './build-task.component.html',
  styleUrls: ['./build-task.component.css']
})
export class BuildTaskComponent implements OnInit {

  @Input() buildTask: BuildTask = null;
  @Output() refreshBuildTask = new EventEmitter();

  constructor() { }

  refresh() {
    this.refreshBuildTask.emit()
  }

  ngOnInit() {
  }

}

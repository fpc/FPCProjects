export class SubBuildTask {
  buildagent: string;
  state: string;
  sourcebuild: boolean;
  uniquestring: string;
  log: string;
}

export class BuildTask {
  packagename: string;
  state: string;
  tag: string;
  uniquestring: string;
  fpcversion: string;
  subtasks: Array<SubBuildTask>;
}

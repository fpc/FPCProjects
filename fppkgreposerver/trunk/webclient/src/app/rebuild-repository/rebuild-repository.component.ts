import { Component, OnInit, Input } from '@angular/core';
import { Repository } from '../repository'
import { FppkgRepositoryService } from '../fppkg-repository.service'
import { HttpErrorResponse } from '@angular/common/http';
import { FPCVersion } from '../fpcversion'

@Component({
  selector: 'app-rebuild-repository',
  templateUrl: './rebuild-repository.component.html',
  styleUrls: ['./rebuild-repository.component.css']
})

export class RebuildRepositoryComponent implements OnInit {

  @Input() repository: Repository;
  @Input() version: FPCVersion;

  isError: boolean = false;
  isBusy: boolean = false;
  errorMsg: string = '';

  constructor(private _fppkgRepositoryService: FppkgRepositoryService) { }

  ngOnInit() {
  }

  rebuildRepository() {
    this._fppkgRepositoryService.rebuildRepository(this.version.name, this.repository.name).subscribe(
      (repositoryManifest) => {
        this.closeError;
        this.isBusy = false;
      },
      (err: HttpErrorResponse) => {
        this.isError = true;
        this.isBusy = false;
        this.errorMsg = 'Call to the Fppkg repository manager failed. ' + err.message;
      }
    )

  }
  private closeError(){
    this.isError = false;
  }
}

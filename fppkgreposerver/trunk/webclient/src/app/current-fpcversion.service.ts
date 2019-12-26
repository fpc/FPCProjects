import { Injectable } from '@angular/core';
import { FPCVersion } from './fpcversion';
import { PackageService } from './package.service';
import { Observable, BehaviorSubject } from 'rxjs';

@Injectable()
export class CurrentFpcversionService {

  private _selectedVersion = new BehaviorSubject<FPCVersion>(null);

  constructor(packageService: PackageService) {
    packageService.getFPCVersionList()
    .subscribe(list => {
      this.setCurrentVersion(list[0]);
    });
  }

  getCurrentVersion(): Observable<FPCVersion> {
    return this._selectedVersion.asObservable();
  }

  setCurrentVersion(version: FPCVersion) {
    this._selectedVersion.next(version);
  }
}

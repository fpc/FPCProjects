import { Component, OnInit, Input } from '@angular/core';
import { Package } from '../package';
import { Observable } from 'rxjs/Observable';
import { of } from 'rxjs/observable/of';
import { NgbActiveModal } from '@ng-bootstrap/ng-bootstrap';
import { PackageService } from '../package.service';
import { Packageversion } from '../packageversion';
import { catchError } from 'rxjs/operators';
import { HttpErrorResponse } from '@angular/common/http';


@Component({
  selector: 'app-add-package',
  templateUrl: './add-package.component.html',
  styleUrls: ['./add-package.component.css']
})
export class AddPackageComponent implements OnInit {

  @Input() packageList: Package[];

  newPackage: Package = {
    name: null,
    ownerid: undefined,
    packagestate: undefined,
    packageversionlist: undefined,
    keywords: undefined,
    category: undefined,
    support: undefined
  };
  errMessage: string = null;

  constructor(
    public activeModal: NgbActiveModal,
    private packageService: PackageService) { }

  ngOnInit() {
  }

  addPackage() {
    this.packageService.addPackage(this.newPackage)
      .pipe(
        catchError(this.handleError(`add package "${this.newPackage.name}"`, null))
      )
      .subscribe(newPackage => {
        if ( newPackage ) {
          this.packageList.push(<Package> newPackage);
          this.activeModal.close('Package added')
        }
      });
  }

  private handleError<T> (operation = 'operation', result?: T) {
    return (error: any): Observable<T> => {
      this.errMessage = error.message;
      console.error(`${operation} failed: ${error.message}`);
      return of(result as T);
    };
  }

}

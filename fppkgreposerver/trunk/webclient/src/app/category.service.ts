import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { OidcSecurityService } from './auth/services/oidc.security.service';
import { Observable } from 'rxjs/Observable';
import { BehaviorSubject } from 'rxjs/BehaviorSubject';
import { map, shareReplay, switchMap } from 'rxjs/operators';
import { environment } from '../environments/environment';
import { Category } from './category';

@Injectable()
export class CategoryService {

  private categoryURL = environment.categoryUrl;
  private _categoryList: Observable<Category[]>;
  private _reload = new BehaviorSubject<void>(null);

  constructor(
    private http: HttpClient,
    private _securityService: OidcSecurityService) {
    }

  getHeaders(): HttpHeaders {
    let authheaders: HttpHeaders;
    let token = this._securityService.getToken();
    if (token !== '') {
      let tokenValue = 'Bearer ' + token;
      authheaders = new HttpHeaders({'authorization': tokenValue});
    } else {
      authheaders = new HttpHeaders();
    }
    return authheaders;
  }

  private requestCategoryList() {
    return this.http.get<Category[]>(`${this.categoryURL}/category`, {headers: this.getHeaders()}).pipe(
      map(response => response)
    );
  }

  public getCategoryList (): Observable<Category[]> {
    if (!this._categoryList) {
      this._categoryList = this._reload.pipe(
        switchMap(_ => this.requestCategoryList()),
        shareReplay(1)
      )
    }
    return this._categoryList;
  }

  public updateCategoryName(category: Category) {
    this.http.put<Category>(`${this.categoryURL}/category/${category.categoryid}`, category, {headers: this.getHeaders()}).subscribe(category => {
      this._reload.next(null);
    });
  }

  public addCategory(category: Category) {
    this.http.post<Category>(`${this.categoryURL}/category`, category, {headers: this.getHeaders()}).subscribe(category => {
      this._reload.next(null);
    });
  }

}

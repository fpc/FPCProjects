import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { OidcSecurityService } from 'angular-auth-oidc-client';
import { Observable } from 'rxjs/Observable';
import { BehaviorSubject } from 'rxjs/BehaviorSubject';
import { map, shareReplay, switchMap } from 'rxjs/operators';
import { Category } from './category';
import { AppConfigService } from './app-config.service';

@Injectable()
export class CategoryService {

  private categoryURL;
  private _categoryList: Observable<Category[]>;
  private _reload = new BehaviorSubject<void>(null);

  constructor(
    private http: HttpClient,
    private appConfigService: AppConfigService,
    private _securityService: OidcSecurityService) {
      this.categoryURL = appConfigService.CategoryUrl;
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
    // Extensive explanation can be found here:
    // https://blog.thoughtram.io/angular/2018/03/05/advanced-caching-with-rxjs.html
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

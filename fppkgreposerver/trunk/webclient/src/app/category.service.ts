import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { OidcSecurityService } from './auth/services/oidc.security.service';
import { Observable } from 'rxjs/Observable';
import { shareReplay } from 'rxjs/operators';
import { environment } from '../environments/environment';
import { Category } from './category';

@Injectable()
export class CategoryService {

  private categoryURL = environment.categoryUrl;
  private _categoryList: Observable<Category[]>;

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

  public getCategoryList (): Observable<Category[]> {
    if (!this._categoryList) {
      this._categoryList = this.http.get<Category[]>(`${this.categoryURL}/category`, {headers: this.getHeaders()}).pipe(shareReplay());
    }
    return this._categoryList;
  }

}

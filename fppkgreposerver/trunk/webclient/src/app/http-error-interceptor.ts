import {Injectable} from '@angular/core';
import {HttpEvent, HttpInterceptor, HttpHandler, HttpRequest, HttpResponse, HttpErrorResponse} from '@angular/common/http';

import {Observable} from 'rxjs';
import {map} from 'rxjs/operators';

@Injectable()
export class HttpErrorInterceptor implements HttpInterceptor {
  intercept(req: HttpRequest<any>, next: HttpHandler): Observable<HttpEvent<any>> {
    return next.handle(req)
      .pipe(map((event: HttpEvent<any>) => {
        if ((event instanceof HttpResponse) && (event.body) && (event.body.error)) {
          throw new Error(event.body.error.msg);
        } else {
          return event;
        }
      }));
  }
}


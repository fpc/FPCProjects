import { TestBed, inject } from '@angular/core/testing';

import { OidcService } from './oidc.service';

describe('OidcService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [OidcService]
    });
  });

  it('should be created', inject([OidcService], (service: OidcService) => {
    expect(service).toBeTruthy();
  }));
});

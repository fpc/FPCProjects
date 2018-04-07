import { TestBed, inject } from '@angular/core/testing';

import { FppkgRepositoryService } from './fppkg-repository.service';

describe('FppkgRepositoryService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [FppkgRepositoryService]
    });
  });

  it('should be created', inject([FppkgRepositoryService], (service: FppkgRepositoryService) => {
    expect(service).toBeTruthy();
  }));
});

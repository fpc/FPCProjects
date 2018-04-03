import { TestBed, inject } from '@angular/core/testing';

import { BuildManagerService } from './build-manager.service';

describe('BuildManagerService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [BuildManagerService]
    });
  });

  it('should be created', inject([BuildManagerService], (service: BuildManagerService) => {
    expect(service).toBeTruthy();
  }));
});

import { TestBed, inject } from '@angular/core/testing';

import { CurrentFpcversionService } from './current-fpcversion.service';

describe('CurrentFpcversionService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [CurrentFpcversionService]
    });
  });

  it('should be created', inject([CurrentFpcversionService], (service: CurrentFpcversionService) => {
    expect(service).toBeTruthy();
  }));
});

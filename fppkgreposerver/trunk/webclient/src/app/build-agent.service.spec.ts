import { TestBed, inject } from '@angular/core/testing';

import { BuildAgentService } from './build-agent.service';

describe('BuildAgentService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [BuildAgentService]
    });
  });

  it('should be created', inject([BuildAgentService], (service: BuildAgentService) => {
    expect(service).toBeTruthy();
  }));
});

import { Test, TestingModule } from '@nestjs/testing';
import { RuntimeService } from './runtime.service';

describe('RuntimeService', () => {
  let service: RuntimeService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [RuntimeService],
    }).compile();

    service = module.get<RuntimeService>(RuntimeService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});

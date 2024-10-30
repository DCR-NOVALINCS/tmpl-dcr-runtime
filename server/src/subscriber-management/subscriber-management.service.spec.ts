import { Test, TestingModule } from '@nestjs/testing';
import { SubscriberManagementService } from './subscriber-management.service';

describe('SubscriberManagementService', () => {
  let service: SubscriberManagementService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [SubscriberManagementService],
    }).compile();

    service = module.get<SubscriberManagementService>(SubscriberManagementService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});

import { Module } from '@nestjs/common';
import { AppController } from '@/app.controller';
import { AppService } from '@/app.service';
import { RuntimeService } from '@/runtime/runtime.service';
import { EventEmitterModule } from '@nestjs/event-emitter';
import { SubscriberManagementService } from './subscriber-management/subscriber-management.service';

@Module({
  imports: [],
  controllers: [AppController],
  providers: [AppService, RuntimeService, SubscriberManagementService],
})
export class AppModule { }

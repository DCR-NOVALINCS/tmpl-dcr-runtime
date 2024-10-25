import { Module } from '@nestjs/common';
import { AppController } from '@/app.controller';
import { AppService } from '@/app.service';
import { RuntimeService } from '@/runtime/runtime.service';
import { EventEmitterModule } from '@nestjs/event-emitter';

@Module({
  imports: [],
  controllers: [AppController],
  providers: [AppService, RuntimeService],
})
export class AppModule { }

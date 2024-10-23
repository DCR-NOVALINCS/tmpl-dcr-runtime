import { Module } from '@nestjs/common';
import { AppController } from '@/app.controller';
import { AppService } from '@/app.service';
import { RuntimeService } from '@/runtime/runtime.service';

@Module({
  imports: [],
  controllers: [AppController],
  providers: [AppService, RuntimeService],
})
export class AppModule { }

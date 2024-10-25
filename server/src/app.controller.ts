import { Body, Controller, Get, Param, Post, Put, Query, Sse, UploadedFile, UseInterceptors } from '@nestjs/common';
import { FileInterceptor } from '@nestjs/platform-express';
import { Observable } from 'rxjs';
import { AppService } from '@/app.service';
import { RuntimeService } from '@/runtime/runtime.service';
import { EventEmitter2 } from '@nestjs/event-emitter';

@Controller()
export class AppController {
  constructor(
    private readonly appService: AppService,
    private readonly runtimeService: RuntimeService
  ) { }

  @Get("ping")
  ping(): string {
    return this.appService.pong();
  }

  @Sse()
  view(@Query("debug") inDebug: boolean = false): Observable<MessageEvent> {

    let viewMessage: () => MessageEvent = () => {
      return new MessageEvent("view", {
        data: this.runtimeService.view(inDebug)
      })
    }

    return new Observable((subscriber) => {
      this.runtimeService.subscribe(subscriber);
      subscriber.next(viewMessage());

      return () => this.runtimeService.unsubscribe(subscriber);
    })
  }

  @Put()
  @UseInterceptors(FileInterceptor("file"))
  updateProgram(@UploadedFile() file: Express.Multer.File): any {
    const content = file.buffer.toString();
    return this.runtimeService.parse(content);
  }

  @Post("event/:id")
  execute(@Param("id") eventId: string, @Body() expr: { data: string }): any {
    return this.runtimeService.execute(eventId, expr.data);
  }
}

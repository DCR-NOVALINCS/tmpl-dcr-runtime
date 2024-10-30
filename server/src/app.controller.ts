import { Body, Controller, Get, Param, Post, Put, Query, Sse, UploadedFile, UseInterceptors } from '@nestjs/common';
import { FileInterceptor } from '@nestjs/platform-express';
import { Observable } from 'rxjs';
import { AppService } from '@/app.service';
import { RuntimeService } from '@/runtime/runtime.service';
import { SubscriberManagementService } from './subscriber-management/subscriber-management.service';

enum EventType {
  Program = "program",
}

@Controller()
export class AppController {
  constructor(
    private readonly appService: AppService,
    private readonly runtimeService: RuntimeService,
    private subscriberSet: SubscriberManagementService
  ) { }

  @Sse("ping")
  ping(): Observable<MessageEvent> {
    return new Observable((subscriber) => {
      console.log("Subscribed to ping");
      setInterval(() => {
        subscriber.next(new MessageEvent("ping", {
          data: this.appService.pong()
        }))
      }, 1000);

      return () => {
        console.log("Unsubscribed from ping");
        subscriber.complete();
      }
    })
  }

  @Sse()
  view(@Query("debug") inDebug: boolean = false): Observable<MessageEvent> {
    console.log("Subscribed to view");
    let viewMessage: () => MessageEvent = () => {
      return new MessageEvent(EventType.Program, {
        data: this.runtimeService.view(inDebug)
      })
    }

    return new Observable((subscriber) => {
      this.subscriberSet.subscribe(subscriber);
      subscriber.next(viewMessage());

      return () => this.subscriberSet.unsubscribe(subscriber);
    })
  }

  @Put()
  @UseInterceptors(FileInterceptor("file"))
  updateProgram(@UploadedFile() file: Express.Multer.File): any {
    const content = file.buffer.toString();
    const res = this.runtimeService.parse(content);
    this.subscriberSet.update((subscriber) => {
      subscriber.next(new MessageEvent("update", {
        data: res
      }))
    });
    return res;
  }

  @Post("event/:id")
  execute(@Param("id") eventId: string, @Body() expr: { data: string }): any {
    return this.runtimeService.execute(eventId, expr.data);
  }
}

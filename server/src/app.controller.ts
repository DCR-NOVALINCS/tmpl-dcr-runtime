import { Body, Controller, Get, Param, Post, Put, Query, Sse } from '@nestjs/common';
import { AppService } from './app.service';
import { RuntimeService } from './runtime/runtime.service';
import { map, Observable, pipe } from 'rxjs';

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
  view(@Query("debug") inDebug: boolean = false): Observable<{ data: string }> {
    return new Observable((subscriber) => {
      subscriber.next({ data: inDebug ? this.runtimeService.debugView() : this.runtimeService.view() });

      return () => subscriber.complete();
    })
    // return inDebug ? this.runtimeService.debugView() : this.runtimeService.view();
  }

  @Put("parse")
  parse(@Body() input: string): any {
    console.log("Parsing: %s", input);
    return this.runtimeService.parse(input);
  }

  @Post("execute/:id")
  execute(@Param("id") eventId: string, @Body() exprString: string): any {
    return this.runtimeService.execute(eventId, exprString);
  }
}

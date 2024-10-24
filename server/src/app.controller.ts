import { Body, Controller, Get, Param, Post, Put, Query, Sse } from '@nestjs/common';
import { AppService } from './app.service';
import { RuntimeService } from './runtime/runtime.service';
import { Observable } from 'rxjs';

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
      subscriber.next({ data: this.runtimeService.view(inDebug) });

      return () => subscriber.complete();
    })
  }

  @Put()
  updateProgram(@Body() input: { data: string }): any {
    console.log("Parsing: %s", input);
    return this.runtimeService.parse(input.data);
  }

  @Post("event/:id")
  execute(@Param("id") eventId: string, @Body() expr: { data: string }): any {
    return this.runtimeService.execute(eventId, expr.data);
  }
}

import { Injectable } from '@nestjs/common';

@Injectable()
export class AppService {
  getHello(): string {
    return 'Hello World!';
  }

  pong(): string {
    let now = new Date();
    return `Pong! ${now.toISOString()}`;
  }
}

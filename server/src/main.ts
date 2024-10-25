import { NestFactory } from '@nestjs/core';
import { AppModule } from './app.module';

async function bootstrap() {
  const app = await NestFactory.create(AppModule);
  const origin_port = process.env.ORIGIN_PORT ?? 4000;
  app.enableCors({
    origin: `http://localhost:${origin_port}`,
  })
  await app.listen(process.env.PORT ?? 3000);
}
bootstrap();

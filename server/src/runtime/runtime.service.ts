import { Injectable, InternalServerErrorException } from '@nestjs/common';
import { EventEmitter2 } from '@nestjs/event-emitter';
import { Subscriber } from 'rxjs';

import * as runtime from '@/runtime/runtime';

// TODO: Add exception types to the runtime and handle them here

@Injectable()
export class RuntimeService {

    constructor() { }

    view(isDebugMode: boolean): string {
        try {
            const result = runtime.view();

            return result;
        } catch (e: any) {
            console.error(e);
            throw InternalServerErrorException;
        }
    }

    parse(input: string): any {
        try {
            const result = runtime.parse(input);

            return result;
        } catch (e: any) {
            console.error("Error: ", e);
            throw InternalServerErrorException;
        }
    }

    execute(eventId: string, exprString: string): any {
        try {
            const result = runtime.execute(eventId, exprString);

            return result;
        } catch (e: any) {
            console.error(e);
            throw InternalServerErrorException;
        }
    }
}

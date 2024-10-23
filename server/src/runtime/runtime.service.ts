import { Inject, Injectable, InternalServerErrorException } from '@nestjs/common';

import * as runtime from '@/runtime/runtime';

@Injectable()
export class RuntimeService {

    view(): string {
        try {
            return runtime.view();
        } catch (e: any) {
            console.error(e);
            throw InternalServerErrorException;
        }
    }

    debugView(): string {
        try {
            return runtime.debugView();
        } catch (e: any) {
            console.error(e);
        }
    }

    parse(input: string): any {
        try {
            return runtime.parse(input);
        } catch (e: any) {
            console.error("Error: ", e);
            console.error(e[3][2])
            throw e;
        }
    }

    execute(eventId: string, exprString: string): any {
        try {
            return runtime.execute(eventId, exprString);
        } catch (e: any) {
            console.error(e);
            throw e;
        }
    }
}

import { Injectable, InternalServerErrorException } from '@nestjs/common';

import * as runtime from '@/runtime/runtime';

@Injectable()
export class RuntimeService {

    view(isDebugMode: boolean): string {
        try {
            return isDebugMode ? runtime.debugView() : runtime.view();
        } catch (e: any) {
            console.error(e);
            throw InternalServerErrorException;
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

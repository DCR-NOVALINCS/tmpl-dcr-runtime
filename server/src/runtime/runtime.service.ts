import { Injectable, InternalServerErrorException } from '@nestjs/common';
import { EventEmitter2 } from '@nestjs/event-emitter';
import { Subscriber } from 'rxjs';

import * as runtime from '@/runtime/runtime';

// TODO: Add exception types to the runtime and handle them here

class SubscriberSet {
    private subscribers: Set<Subscriber<any>>;

    constructor() {
        this.subscribers = new Set<Subscriber<any>>();
    }

    add(subscriber: Subscriber<any>): SubscriberSet {
        this.subscribers.add(subscriber);
        return this;
    }

    remove(subscriber: Subscriber<any>): SubscriberSet {
        this.subscribers.delete(subscriber);
        return this;
    }

    foreach(callback: (subscriber: Subscriber<any>) => void): void {
        this.subscribers.forEach(callback);
    }
}

@Injectable()
export class RuntimeService {

    private subscriberSet: SubscriberSet

    constructor() {
        this.subscriberSet = new SubscriberSet();
    }

    view(isDebugMode: boolean): string {
        try {
            const result = runtime.view();

            return result;
            // return isDebugMode ? runtime.debugView() : runtime.view();
        } catch (e: any) {
            console.error(e);
            throw InternalServerErrorException;
        }
    }

    parse(input: string): any {
        try {
            const result = runtime.parse(input);
            this.propagateUpdate();

            return result;
        } catch (e: any) {
            console.error("Error: ", e);
            throw InternalServerErrorException;
        }
    }

    execute(eventId: string, exprString: string): any {
        try {
            const result = runtime.execute(eventId, exprString);
            this.propagateUpdate();

            return result;
        } catch (e: any) {
            console.error(e);
            throw InternalServerErrorException;
        }
    }

    // FIXME: This is not the place to handle subscriptions

    propagateUpdate(inDebug: boolean = false): void {
        this.subscriberSet.foreach(subscriber => {
            subscriber.next(this.view(inDebug));
        });
    }

    subscribe(subscriber: Subscriber<any>): void {
        this.subscriberSet.add(subscriber);
    }

    unsubscribe(subscriber: Subscriber<any>): void {
        this.subscriberSet.remove(subscriber);
        return subscriber.complete();
    }
}

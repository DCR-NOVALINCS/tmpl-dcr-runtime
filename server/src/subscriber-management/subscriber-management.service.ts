import { Injectable } from '@nestjs/common';
import { Subscriber } from 'rxjs';

@Injectable()
export class SubscriberManagementService {

    private subscribers: Set<Subscriber<any>> = new Set<Subscriber<any>>();
    // private callback: (subscriber: Subscriber<any>) => void = () => { };

    constructor(
        // callback: (subscriber: Subscriber<any>) => void
    ) {
        // this.callback = callback;
    }

    subscribe(subscriber: Subscriber<any>): SubscriberManagementService {
        this.subscribers.add(subscriber);
        return this;
    }

    unsubscribe(subscriber: Subscriber<any>): SubscriberManagementService {
        this.subscribers.delete(subscriber);
        subscriber.complete();
        return this;
    }

    update(callback: (subscriber: Subscriber<any>) => void): void {
        this.subscribers.forEach(subscriber => {
            // this.callback(subscriber);
            callback(subscriber);
        });
    }
}

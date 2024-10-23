// Type definitions for runtime.js

export function view(): string;

export function debugView(): string;

export function parse(input: string): any;

export function execute(eventId: string, exprString: string): any;
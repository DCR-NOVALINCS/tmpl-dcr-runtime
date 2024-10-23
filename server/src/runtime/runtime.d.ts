// Type definitions for runtime.js

/**
 * Returns a string representation of the current state of the program
 * @param input The input string to parse
 */
export function view(): string;

/**
 * Returns a string representation of the current state of the program with all the elements
 * @param input The input string to parse
 */
export function debugView(): string;

export function parse(input: string): any;

export function execute(eventId: string, exprString: string): any;

// declare module 'runtime' {
//     const runtime: {
//         view: () => string;
//         debugView: () => string;
//         parse: (input: string) => any;
//         execute: (eventId: string, exprString: string) => any;
//     };
// }
// export default {
//     view,
//     debugView,
//     parse,
//     execute
// };
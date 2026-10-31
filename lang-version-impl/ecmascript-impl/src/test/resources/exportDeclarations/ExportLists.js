const a = 1, b = 2;
export { a, b as c };
export { a as default, };
export { x, default as y } from './x';
export * from './all';
export * as ns from "./ns";

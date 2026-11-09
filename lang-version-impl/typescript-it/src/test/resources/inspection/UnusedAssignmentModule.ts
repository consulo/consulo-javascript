import { x } from './other';

let <warning descr="com.sixrr.inspectjs.InspectionJSLocalize@unused.assignment.error.string">local</warning> = 1;
local = 2;
export let exported = 1;
exported = 2;
console.log(local, exported, x);

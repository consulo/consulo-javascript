declare let b: boolean;
interface I {
    a: number;
}
type WithUndefined = number | undefined;
type Name = string;
enum E {
    A
}

function lacksReturn(): <error descr="com.sixrr.inspectjs.InspectionJSLocalize@function.lacks.ending.return.error.string">number</error> {
    if (b) return 1;
}
function noReturn(): <error descr="com.sixrr.inspectjs.InspectionJSLocalize@function.must.return.value.error.string">string</error> {
}
function neverEnds(): <error descr="com.sixrr.inspectjs.InspectionJSLocalize@function.returning.never.error.string">never</error> {
}
function acceptsUndefined(): number | undefined {
    if (b) return 1;
}
function unknownNoReturn(): <error descr="com.sixrr.inspectjs.InspectionJSLocalize@function.must.return.value.error.string">unknown</error> {
}
function voidOk(): void {
    if (b) return;
}
function anyOk(): any {
}
function aliasWithUndefined(): WithUndefined {
    if (b) return 1;
}
function aliasName(): <error descr="com.sixrr.inspectjs.InspectionJSLocalize@function.lacks.ending.return.error.string">Name</error> {
    if (b) return "a";
}
function typeParameter<X>(x: X): <error descr="com.sixrr.inspectjs.InspectionJSLocalize@function.lacks.ending.return.error.string">X</error> {
    if (b) return x;
}
function interfaceType(): <error descr="com.sixrr.inspectjs.InspectionJSLocalize@function.lacks.ending.return.error.string">I</error> {
    if (b) return { a: 1 };
}
function enumType(): <error descr="com.sixrr.inspectjs.InspectionJSLocalize@function.lacks.ending.return.error.string">E</error> {
    if (b) return E.A;
}
function nullType(): <error descr="com.sixrr.inspectjs.InspectionJSLocalize@function.lacks.ending.return.error.string">null</error> {
    if (b) return null;
}
function arrayType(): <error descr="com.sixrr.inspectjs.InspectionJSLocalize@function.lacks.ending.return.error.string">number[]</error> {
    if (b) return [];
}
function literalType(): <error descr="com.sixrr.inspectjs.InspectionJSLocalize@function.lacks.ending.return.error.string">'a' | 'b'</error> {
    if (b) return 'a';
}
function predicate(x: unknown): <error descr="com.sixrr.inspectjs.InspectionJSLocalize@function.lacks.ending.return.error.string">x is string</error> {
    if (b) return true;
}
async function promised(): <error descr="com.sixrr.inspectjs.InspectionJSLocalize@function.lacks.ending.return.error.string">Promise<number></error> {
    if (b) return 1;
}
async function promisedVoid(): Promise<void> {
    if (b) return;
}
function* generator(): <error descr="com.sixrr.inspectjs.InspectionJSLocalize@function.must.return.value.error.string">Generator<number, number></error> {
    yield 1;
}
function allPaths(): number {
    if (b) {
        return 1;
    } else {
        return 2;
    }
}
function throws(): number {
    if (b) return 1;
    throw new Error();
}
function loopsForever(): number {
    for (;;) {
    }
}
function tryCatch(): <error descr="com.sixrr.inspectjs.InspectionJSLocalize@function.lacks.ending.return.error.string">number</error> {
    try {
        return 1;
    } catch {
    }
}
function switchDefault(): number {
    switch (b) {
        case true:
            return 1;
        default:
            return 2;
    }
}
const arrow = (): <error descr="com.sixrr.inspectjs.InspectionJSLocalize@function.lacks.ending.return.error.string">number</error> => {
    if (b) return 1;
};
const expressionBody = (): number => 1;
const object = {
    m(): <error descr="com.sixrr.inspectjs.InspectionJSLocalize@function.lacks.ending.return.error.string">number</error> {
        if (b) return 1;
    }
};
class K {
    get <error descr="com.sixrr.inspectjs.InspectionJSLocalize@get.accessor.must.return.value.error.string">untyped</error>() {
    }
    get <error descr="com.sixrr.inspectjs.InspectionJSLocalize@get.accessor.must.return.value.error.string">typed</error>(): <error descr="com.sixrr.inspectjs.InspectionJSLocalize@function.must.return.value.error.string">number</error> {
    }
    get returns(): <error descr="com.sixrr.inspectjs.InspectionJSLocalize@function.lacks.ending.return.error.string">number</error> {
        if (b) return 1;
    }
    method(): <error descr="com.sixrr.inspectjs.InspectionJSLocalize@function.lacks.ending.return.error.string">string</error> {
        if (b) return "";
    }
}

declare function use(...a: any[]): void;
declare function risky(): void;
declare let c: boolean;

function overwritten() {
    let <warning descr="com.sixrr.inspectjs.InspectionJSLocalize@unused.assignment.error.string">x</warning> = 1;
    x = 2;
    use(x);
}
function lastWrite() {
    let i = 0;
    use(i);
    <warning descr="com.sixrr.inspectjs.InspectionJSLocalize@unused.assignment.error.string">i</warning>++;
}
function onePath() {
    let x = 0;
    if (c) x = 1;
    use(x);
}
function loop() {
    let total = 0;
    for (let i = 0; i < 3; i++) {
        total += i;
    }
    use(total);
}
function compound() {
    let x = 1;
    <warning descr="com.sixrr.inspectjs.InspectionJSLocalize@unused.assignment.error.string">x</warning> += 2;
}
function tryCatch() {
    let state = 0;
    try {
        state = 1;
        risky();
        state = 2;
    } catch {
        use(state);
    }
}
function finallyRead() {
    let x = 0;
    try {
        x = 1;
    } finally {
        use(x);
    }
}
function captured() {
    let x = 1;
    const f = () => x;
    x = 2;
    use(f);
}
function neverRead() {
    let unused = 1;
    unused = 2;
}
function parameter(p: number) {
    use(p);
    <warning descr="com.sixrr.inspectjs.InspectionJSLocalize@unused.assignment.error.string">p</warning> = 1;
}
function destructuring() {
    let <warning descr="com.sixrr.inspectjs.InspectionJSLocalize@unused.assignment.error.string">a</warning> = 0, <warning descr="com.sixrr.inspectjs.InspectionJSLocalize@unused.assignment.error.string">b</warning> = 0;
    [a, b] = [1, 2];
    use(a, b);
}
function loopCarried() {
    let previous = 0;
    for (let i = 0; i < 3; i++) {
        use(previous);
        previous = i;
    }
}
let global = 1;
global = 2;
use(global);

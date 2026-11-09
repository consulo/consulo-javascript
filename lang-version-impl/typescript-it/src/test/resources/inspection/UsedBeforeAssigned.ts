declare let c: boolean;
declare function use(...a: any[]): void;

function basic() {
    let x: number;
    if (c) x = 1;
    use(<error descr="com.sixrr.inspectjs.InspectionJSLocalize@variable.used.before.assigned.error.string">x</error>);
}
function allPaths() {
    let x: number;
    if (c) x = 1; else x = 2;
    use(x);
}
function untyped() {
    let x;
    use(x);
}
function withUndefined() {
    let x: number | undefined;
    use(x);
}
function anyType() {
    let x: any;
    use(x);
}
function tdz() {
    use(<error descr="com.sixrr.inspectjs.InspectionJSLocalize@variable.used.before.assigned.error.string">y</error>);
    let y = 1;
}
function tdzInFunction() {
    const f = () => y;
    let y = 1;
    use(f);
}
function selfInit() {
    let z: number = <error descr="com.sixrr.inspectjs.InspectionJSLocalize@variable.used.before.assigned.error.string">z</error> + 1;
}
function varHoisted() {
    use(<error descr="com.sixrr.inspectjs.InspectionJSLocalize@variable.used.before.assigned.error.string">v</error>);
    var v: number = 1;
}
function nonNull() {
    let x: number;
    use(x!);
}
function definite() {
    let x!: number;
    use(x);
}
function closureWrite() {
    let x: number;
    const init = () => { x = 1; };
    init();
    use(<error descr="com.sixrr.inspectjs.InspectionJSLocalize@variable.used.before.assigned.error.string">x</error>);
}
function closureRead() {
    let x: number;
    const read = () => <error descr="com.sixrr.inspectjs.InspectionJSLocalize@variable.used.before.assigned.error.string">x</error>;
    use(read);
}
function loops() {
    let x: number;
    while (c) {
        x = 1;
    }
    use(<error descr="com.sixrr.inspectjs.InspectionJSLocalize@variable.used.before.assigned.error.string">x</error>);
    let y: number;
    for (;;) {
        y = 1;
        break;
    }
    use(y);
}
function tryCatch() {
    let x: number;
    try {
        x = 1;
    } catch {
        use(<error descr="com.sixrr.inspectjs.InspectionJSLocalize@variable.used.before.assigned.error.string">x</error>);
    }
    use(<error descr="com.sixrr.inspectjs.InspectionJSLocalize@variable.used.before.assigned.error.string">x</error>);
    let z: number;
    try {
        z = 1;
    } finally {
    }
    use(z);
}
function logical() {
    let x: number;
    if (c && (x = 1)) {
        use(x);
    }
    let y: number;
    c || (y = 1);
    use(<error descr="com.sixrr.inspectjs.InspectionJSLocalize@variable.used.before.assigned.error.string">y</error>);
}
function typeofUse() {
    let x: number;
    use(typeof <error descr="com.sixrr.inspectjs.InspectionJSLocalize@variable.used.before.assigned.error.string">x</error>);
}
function compound() {
    let x: number;
    <error descr="com.sixrr.inspectjs.InspectionJSLocalize@variable.used.before.assigned.error.string">x</error> += 1;
    let y: number;
    <error descr="com.sixrr.inspectjs.InspectionJSLocalize@variable.used.before.assigned.error.string">y</error>++;
}
function destructuring() {
    let a: number, b: number;
    [a, b] = [1, 2];
    use(a, b);
    let p: number;
    ({ p } = { p: 1 });
    use(p);
}
function forOf(arr: number[]) {
    let last: number;
    for (last of arr) {
    }
    use(<error descr="com.sixrr.inspectjs.InspectionJSLocalize@variable.used.before.assigned.error.string">last</error>);
}
function stringLiteral() {
    use(<error descr="com.sixrr.inspectjs.InspectionJSLocalize@variable.used.before.assigned.error.string">s</error>);
    let s = "a";
}
function classUse() {
    new K();
    class K {}
}
function switchCase(n: number) {
    let x: number;
    switch (n) {
        case 1: x = 1; break;
        default: x = 2;
    }
    use(x);
}

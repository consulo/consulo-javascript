declare let c: boolean;
declare function use(...a: any[]): void;
declare function get(): number | undefined;

function doWhile() {
    let x: number;
    do {
        x = 1;
    } while (c);
    use(x);
}
function labeledBlock() {
    let x: number;
    out: {
        if (c) {
            x = 1;
            break out;
        }
        x = 2;
    }
    use(x);
}
function nullish() {
    let x: number;
    x = get() ?? 1;
    use(x);
    let y: number | undefined;
    y ??= 1;
    use(y);
}
function fallThrough(n: number) {
    let x: number;
    switch (n) {
        case 1:
        case 2:
            x = 1;
            break;
        case 3:
            return;
        default:
            throw new Error();
    }
    use(x);
}
function noDefault(n: number) {
    let x: number;
    switch (n) {
        case 1:
            x = 1;
    }
    use(<error descr="com.sixrr.inspectjs.InspectionJSLocalize@variable.used.before.assigned.error.string">x</error>);
}
function nestedClosures() {
    const k: number = 1;
    let late: number;
    const outer = () => () => late;
    late = 2;
    const g = () => k;
    use(outer, g);
}
function loopAssignment() {
    let x: number;
    const read = () => x;
    for (let i = 0; i < 2; i++) {
        x = i;
    }
    use(read);
}
function conditional() {
    let x: number;
    const y = c ? (x = 1) : (x = 2);
    use(x, y);
    let z: number;
    if (!(c || (z = 1))) {
        use(z);
    }
}
function objectMethod() {
    let x: number;
    const o = {
        m() {
            return <error descr="com.sixrr.inspectjs.InspectionJSLocalize@variable.used.before.assigned.error.string">x</error>;
        }
    };
    use(o);
}

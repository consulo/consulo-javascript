declare function f(): void;
declare let a: any, b: any, c: any;

function ifElse(): void {
    if (a) {
        f();
    } else if (b) {
        return;
    } else {
        f();
    }
    f();
}

function shortCircuit(): void {
    if (a && b || !c) {
        f();
    }
    if (a ?? b) {
        f();
    }
}

function literals(): void {
    if (false) {
        f();
    }
    if (a && false) {
        f();
    }
    if (true || a) {
        f();
    } else {
        f();
    }
    if ((false)) {
        f();
    }
    if (!true) {
        f();
    }
    if (a ?? false) {
        f();
    }
}

function values(): void {
    const x = a ? b : c;
    const y = a?.b.c(() => b);
    a ||= () => c;
}

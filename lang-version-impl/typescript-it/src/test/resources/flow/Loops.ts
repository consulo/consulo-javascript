declare function f(): void;
declare let a: any, b: any;

function loops(): void {
    while (a) {
        f();
    }
    do {
        f();
    } while (a);
    for (let i = 0; i < 3; i++) {
        f();
    }
    for (const k in a) {
        f();
    }
    for (const v of a) {
        f();
    }
    f();
}

function infinite(): void {
    while (true) {
        f();
    }
    f();
}

function forever(): void {
    for (;;) {
        if (a) break;
    }
    f();
}

function labels(): void {
    outer: for (const x of a) {
        for (const y of b) {
            if (y) continue outer;
            if (x) break outer;
            continue;
        }
        f();
    }
    block: {
        if (a) break block;
        f();
    }
    f();
}

function doFalse(): void {
    do {
        f();
    } while (false);
    f();
}

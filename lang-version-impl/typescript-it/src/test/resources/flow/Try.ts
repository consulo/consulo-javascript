declare function f(): void;
declare let a: any;

function catchIt(): void {
    try {
        f();
    } catch (e) {
        return;
    }
    f();
}

function returnThroughFinally(): void {
    try {
        return;
    } finally {
        f();
    }
    f();
}

function jumpsThroughFinally(): void {
    while (a) {
        try {
            if (a) break;
            if (a) continue;
            f();
        } catch {
            throw a;
        } finally {
            f();
        }
    }
}

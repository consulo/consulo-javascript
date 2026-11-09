declare function f(): void;

const arrow = (x: number) => x + 1;
const block = async (x: number) => {
    await f();
};
const expression = function named() {
    return 1;
};
class C {
    static s = 1;
    static {
        f();
    }
    constructor(private p: number = 1) {
    }
    get g(): number {
        return 1;
    }
    method(): void {
        function inner() {
            f();
        }
        inner();
    }
}
const o = {
    m() {
        f();
    },
    p: () => 1
};
f();

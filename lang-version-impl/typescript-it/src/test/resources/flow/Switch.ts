declare function f(): void;
declare let a: any;

function fallThrough(): void {
    switch (a) {
        case 1:
            f();
        case 2:
            f();
            break;
        default:
            f();
        case 3:
            return;
    }
    f();
}

function noDefault(): void {
    switch (a) {
        case 1:
            return;
        case 2:
            return;
    }
    f();
}

function allReturn(): number {
    switch (a) {
        case 1:
            return 1;
        default:
            return 2;
    }
    f();
}

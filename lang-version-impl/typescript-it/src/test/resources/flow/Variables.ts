declare function use(...a: any[]): void;
declare let c: boolean;

function declarations(p: number = 1, { q }: any): void {
    let x: number;
    var y;
    const [d1, d2] = [p, q];
    const { e1, e2: e3 = d1 } = { e1: 1, e2: 2 };
    use(x, y, d2, e1, e3);
}

function assignments(): void {
    let x = 0, y = 0, z = 0;
    x = y + 1;
    x += 1;
    x++;
    --x;
    y ||= 2;
    [x, y] = [y, x];
    ({ z } = { z: 1 });
    use(typeof x, z);
}

function loopsAndScopes(): void {
    while (c) {
        let fresh: number;
        if (c) fresh = 1;
        {
            let fresh = 2;
            use(fresh);
        }
    }
    for (const v of [1]) {
        use(v);
    }
    let last: number;
    for (last of [1]) {
    }
    try {
        last = 1;
    } catch (e) {
        use(e);
    }
}

function closures(): void {
    let x = 1;
    const read = () => x;
    function write() {
        x = 2;
    }
    use(read, write);
}

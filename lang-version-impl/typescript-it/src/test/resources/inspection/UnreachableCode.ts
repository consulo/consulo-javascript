declare function f(): void;
declare let a: boolean;

function afterReturn(): void {
    return;
    <warning descr="com.sixrr.inspectjs.InspectionJSLocalize@unreachable.code.error.string">f();
    let x = 1;</warning>
    var y;
    <warning descr="com.sixrr.inspectjs.InspectionJSLocalize@unreachable.code.error.string">f();</warning>
    function g() {
    }
    <warning descr="com.sixrr.inspectjs.InspectionJSLocalize@unreachable.code.error.string">f();</warning>
}

function constantIf(): void {
    if (false) {
        <warning descr="com.sixrr.inspectjs.InspectionJSLocalize@unreachable.code.error.string">f();
        f();</warning>
    }
    if (a && false) <warning descr="com.sixrr.inspectjs.InspectionJSLocalize@unreachable.code.error.string">f();</warning>
}

function infinite(): void {
    while (true) {
    }
    <warning descr="com.sixrr.inspectjs.InspectionJSLocalize@unreachable.code.error.string">if (a) {
        f();
    }
    const arrow = () => {
        return;
        <warning descr="com.sixrr.inspectjs.InspectionJSLocalize@unreachable.code.error.string">f();</warning>
    };</warning>
}

function finallyReturn(): void {
    try {
        return;
    } finally {
        f();
    }
    <warning descr="com.sixrr.inspectjs.InspectionJSLocalize@unreachable.code.error.string">f();</warning>
}

function switchReturn(): number {
    switch (a) {
        case true:
            return 1;
        default:
            return 2;
    }
    <warning descr="com.sixrr.inspectjs.InspectionJSLocalize@unreachable.code.error.string">f();</warning>
}

function loopBreak(): void {
    for (;;) {
        break;
        <warning descr="com.sixrr.inspectjs.InspectionJSLocalize@unreachable.code.error.string">f();</warning>
    }
    do {
        continue;
        <warning descr="com.sixrr.inspectjs.InspectionJSLocalize@unreachable.code.error.string">f();</warning>
    } while (a);
}

class K {
}
throw 1;
<warning descr="com.sixrr.inspectjs.InspectionJSLocalize@unreachable.code.error.string">f();
class Z {
}</warning>

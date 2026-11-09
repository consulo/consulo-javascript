function <warning descr="com.sixrr.inspectjs.InspectionJSLocalize@function.has.inconsistent.return.points.error.string">partial</warning>(a) {
    if (a) return 1;
}
function allPaths(a) {
    if (a) return 1;
    else return 2;
}
function loop(a) {
    while (true) {
        if (a) return 1;
    }
}
function tryFinally(a) {
    try {
        return 1;
    } finally {
        a++;
    }
}
function switchAll(a) {
    switch (a) {
        case 1:
            return 1;
        default:
            throw new Error();
    }
}
function labeled(a) {
    out: {
        if (a) break out;
        return 1;
    }
    return 2;
}

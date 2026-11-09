function f() {
    return 1;
}
var a = f();
if (a) {
    a = 2;
} else {
    a = f() && 3;
}
with (a) {
    f();
}
class D {
    static {
        f();
    }
    m() {
        return a?.b;
    }
}
const g = (x) => x;
for (const v of a) {
    f();
}

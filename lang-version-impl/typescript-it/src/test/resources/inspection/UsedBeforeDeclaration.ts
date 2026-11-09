declare function use(...a: any[]): void;

function beforeUse() {
    use(<error descr="com.sixrr.inspectjs.InspectionJSLocalize@block.scoped.variable.used.before.declaration.error.string">a</error>);
    <error descr="com.sixrr.inspectjs.InspectionJSLocalize@block.scoped.variable.used.before.declaration.error.string">a</error> = 2;
    let a = 1;
    use(a);
}
function ownInitializer() {
    const b: number = <error descr="com.sixrr.inspectjs.InspectionJSLocalize@block.scoped.variable.used.before.declaration.error.string">b</error> + 1;
    const [c, d] = [<error descr="com.sixrr.inspectjs.InspectionJSLocalize@block.scoped.variable.used.before.declaration.error.string">d</error>, 1];
}
function deferred() {
    const f = () => e;
    function g() {
        return e;
    }
    const e = 1;
    use(f, g);
}
function immediately() {
    (function () {
        use(<error descr="com.sixrr.inspectjs.InspectionJSLocalize@block.scoped.variable.used.before.declaration.error.string">h</error>);
    })();
    const h = 1;
}
function classAndEnum() {
    new <error descr="com.sixrr.inspectjs.InspectionJSLocalize@class.used.before.declaration.error.string">K</error>();
    use(<error descr="com.sixrr.inspectjs.InspectionJSLocalize@enum.used.before.declaration.error.string">E</error>.A);
    class K {
    }
    enum E {
        A
    }
}
function typeofUse() {
    use(typeof <error descr="com.sixrr.inspectjs.InspectionJSLocalize@block.scoped.variable.used.before.declaration.error.string">t</error>);
    let t = 1;
}
function varIsHoisted() {
    use(v);
    var v = 1;
}
class Fields {
    instance = later;
    static stat = <error descr="com.sixrr.inspectjs.InspectionJSLocalize@block.scoped.variable.used.before.declaration.error.string">later</error>;
}
const later = 1;
use(<error descr="com.sixrr.inspectjs.InspectionJSLocalize@block.scoped.variable.used.before.declaration.error.string">topLevel</error>);
let topLevel = 2;

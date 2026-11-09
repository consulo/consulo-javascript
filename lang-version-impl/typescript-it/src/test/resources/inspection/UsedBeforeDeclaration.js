function beforeUse(f) {
    f(<error descr="com.sixrr.inspectjs.InspectionJSLocalize@block.scoped.variable.used.before.declaration.error.string">a</error>, <error descr="com.sixrr.inspectjs.InspectionJSLocalize@block.scoped.variable.used.before.declaration.error.string">b</error>);
    let a = 1;
    const b = new <error descr="com.sixrr.inspectjs.InspectionJSLocalize@class.used.before.declaration.error.string">K</error>();
    class K {
    }
    const later = () => c;
    const c = 1;
    return later;
}

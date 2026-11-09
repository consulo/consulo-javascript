function afterThrow(a) {
    if (a) {
        throw new Error();
        <warning descr="com.sixrr.inspectjs.InspectionJSLocalize@unreachable.code.error.string">a = 1;
        a++;</warning>
    }
    label: {
        break label;
        <warning descr="com.sixrr.inspectjs.InspectionJSLocalize@unreachable.code.error.string">a = 2;</warning>
    }
    return a;
}

class C {
    static {
        throw new Error();
        <warning descr="com.sixrr.inspectjs.InspectionJSLocalize@unreachable.code.error.string">C.x = 1;</warning>
    }
    m() {
        return 1;
        <warning descr="com.sixrr.inspectjs.InspectionJSLocalize@unreachable.code.error.string">this.x = 1;</warning>
    }
}

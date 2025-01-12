/**
 * @author VISTALL
 * @since 23-Aug-22
 */
open module consulo.javascript.base.api
{
    requires transitive consulo.ide.api;

    exports com.intellij.lang.javascript.formatter;
    exports com.intellij.lang.javascript.psi;
    exports com.intellij.lang.javascript.psi.stubs;

    exports consulo.javascript.icon;
    exports consulo.javascript.ide.completion;
    exports consulo.javascript.index;
    exports consulo.javascript.language;
    exports consulo.javascript.language.psi;
    exports consulo.javascript.localize;
    exports consulo.javascript.module.extension;
    exports consulo.javascript.psi;
    exports consulo.javascript.psi.impl.reference;
    exports consulo.javascript.language.psi.stub;
    exports consulo.javascript.psi.stubs;

    exports consulo.javascript.internal to consulo.javascript.ecmascript.impl, consulo.javascript.base.impl;
}
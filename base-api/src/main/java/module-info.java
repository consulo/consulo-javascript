/**
 * @author VISTALL
 * @since 2022-08-23
 */
open module consulo.javascript.base.api {
    requires transitive consulo.language.api;
    requires transitive consulo.language.editor.api;
    requires transitive consulo.language.code.style.api;
    requires consulo.module.creation.api;

    exports com.intellij.lang.javascript.formatter;
    exports com.intellij.lang.javascript.psi;
    exports com.intellij.lang.javascript.psi.ecmal4;
    exports com.intellij.lang.javascript.patterns;
    exports com.intellij.lang.javascript.psi.stubs;

    exports consulo.javascript.icon;
    exports consulo.javascript.ide.completion;
    exports consulo.javascript.index;
    exports consulo.javascript.language;
    exports consulo.javascript.library;
    exports consulo.javascript.language.psi;
    exports consulo.javascript.language.psi.stub;
    exports consulo.javascript.localize;
    exports consulo.javascript.module.declarations;
    exports consulo.javascript.module.extension;
    exports consulo.javascript.module.resolve;
    exports consulo.javascript.newProjectOrModule;
    exports consulo.javascript.psi;
    exports consulo.javascript.psi.impl.reference;
    exports consulo.javascript.psi.resolve;
    exports consulo.javascript.psi.stubs;

    exports consulo.javascript.internal to
        consulo.javascript.base.impl,
        consulo.javascript.ecmascript.impl;
}
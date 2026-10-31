/**
 * @author VISTALL
 * @since 29-Aug-22
 */
module consulo.javascript.typescript.impl {
    requires consulo.javascript.base.impl;
    requires consulo.javascript.ecmascript.impl;

    requires consulo.language.impl;
    requires com.intellij.xml.html.api;

    exports consulo.typescript;
    exports consulo.typescript.version;
    exports consulo.typescript.language;
    exports consulo.typescript.language.psi;
    exports consulo.typescript.language.impl;
    exports consulo.typescript.language.impl.psi;
    exports consulo.typescript.language.impl.psi.mixin;
    exports consulo.typescript.language.impl.stub;
    exports consulo.typescript.html;
}
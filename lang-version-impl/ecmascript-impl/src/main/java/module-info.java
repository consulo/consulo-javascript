/**
 * @author VISTALL
 * @since 23-Aug-22
 */
module consulo.javascript.ecmascript.impl
{
    requires transitive consulo.javascript.base.impl;

    requires consulo.language.editor.impl;

    requires com.intellij.xml;

    exports consulo.javascript.ecmascript.codeInsight;
    exports consulo.javascript.ecmascript.codeInsight.quickFixes;
    exports consulo.javascript.ecmascript.completion;
    exports consulo.javascript.ecmascript.lang;
    exports consulo.javascript.ecmascript.lang.parsing;
    exports consulo.javascript.ecmascript.psi;
    exports consulo.javascript.ecmascript.psi.impl;
    exports consulo.javascript.ecmascript.psi.impl.resolve;
    exports consulo.javascript.jsx.fileTemplate;
    exports consulo.javascript.jsx.language;
    exports consulo.javascript.jsx.language.psi.impl;
}
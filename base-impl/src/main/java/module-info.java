/**
 * @author VISTALL
 * @since 23-Aug-22
 */
open module consulo.javascript.base.impl {
    requires transitive consulo.javascript.base.api;

    requires com.google.common;
    requires com.intellij.xml;

    exports com.intellij.javascript.documentation;
    exports com.intellij.lang.javascript;
    exports com.intellij.lang.javascript.flex;
    exports com.intellij.lang.javascript.highlighting;
    exports com.intellij.lang.javascript.index;
    exports com.intellij.lang.javascript.inspections.qucikFixes;
    exports com.intellij.lang.javascript.parsing;
    exports com.intellij.lang.javascript.psi.impl;
    exports com.intellij.lang.javascript.psi.impl.reference;
    exports com.intellij.lang.javascript.psi.resolve;
    exports com.intellij.lang.javascript.psi.stubs.impl;
    exports com.intellij.lang.javascript.psi.util;
    exports com.intellij.lang.javascript.search;
    exports com.intellij.lang.javascript.types;
    exports com.intellij.lang.javascript.validation;
    exports consulo.javascript.ecmascript4.psi.impl;
    exports consulo.javascript.ide.codeInsight;
    exports consulo.javascript.ide.hightlight;
    exports consulo.javascript.impl.language.psi;
    exports consulo.javascript.impl.language.psi.stub;
    exports consulo.javascript.impl.language;
    exports consulo.javascript.inspections.qucikFixes;
    exports consulo.javascript.lang;
    exports consulo.javascript.lang.lexer;
    exports consulo.javascript.lang.parsing;
    exports consulo.javascript.lang.parsing.impl;
    exports consulo.javascript.lang.psi.impl;
    exports consulo.javascript.lang.psi.impl.elementType;
    exports consulo.javascript.lang.psi.impl.resolve;
    exports consulo.javascript.lang.viewProvider;
    exports consulo.javascript.types;
}
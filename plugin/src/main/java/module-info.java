/**
 * @author VISTALL
 * @since 23-Aug-22
 */
open module consulo.javascript {
    requires transitive consulo.javascript.base.api;
    requires transitive consulo.javascript.base.impl;

    requires consulo.language.editor.impl;

    requires com.intellij.xml;

    // TODO remove in future
    requires java.desktop;
    requires forms.rt;

    exports com.intellij.javascript;
    exports com.intellij.javascript.manipulators;
    exports com.intellij.lang.javascript.impl;
    exports com.intellij.lang.javascript.impl.findUsages;
    exports com.intellij.lang.javascript.impl.flex;
    exports com.intellij.lang.javascript.impl.flex.importer;
    exports com.intellij.lang.javascript.impl.folding;
    exports com.intellij.lang.javascript.impl.formatter;
    exports com.intellij.lang.javascript.impl.formatter.blocks;
    exports com.intellij.lang.javascript.impl.generation;
    exports com.intellij.lang.javascript.impl.highlighting;
    exports com.intellij.lang.javascript.impl.index.predefined;
    exports com.intellij.lang.javascript.impl.inspections;
    exports com.intellij.lang.javascript.impl.navigation;
    exports com.intellij.lang.javascript.impl.refactoring;
    exports com.intellij.lang.javascript.impl.refactoring.extractMethod;
    exports com.intellij.lang.javascript.impl.refactoring.introduceConstant;
    exports com.intellij.lang.javascript.impl.refactoring.introduceField;
    exports com.intellij.lang.javascript.impl.refactoring.introduceVariable;
    exports com.intellij.lang.javascript.impl.search;
    exports com.intellij.lang.javascript.impl.structureView;
    exports com.intellij.lang.javascript.impl.surroundWith;
    exports consulo.javascript.impl;
    exports consulo.javascript.impl.copyright;
    exports consulo.javascript.impl.findUsages;
    exports consulo.javascript.impl.formatter;
    exports consulo.javascript.impl.ide.actions;
    exports consulo.javascript.impl.ide.completion;
    exports consulo.javascript.impl.ide.navigationToolbar;
    exports consulo.javascript.impl.lang;
    exports consulo.javascript.impl.lang.navigation;
    exports consulo.javascript.impl.psi.impl.reference;
    exports com.intellij.lang.javascript.impl.validation;
}
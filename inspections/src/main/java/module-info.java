/**
 * @author VISTALL
 * @since 29-Aug-22
 */
open module consulo.javascript.inspections {
    // TODO remove in future
    requires java.desktop;

    requires com.intellij.xml.api;

    requires consulo.language.editor.refactoring.api;

    requires consulo.javascript.base.impl;

    requires consulo.javascript.ecmascript.impl;
}
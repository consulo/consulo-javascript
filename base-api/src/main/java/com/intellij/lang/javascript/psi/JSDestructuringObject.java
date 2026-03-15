package com.intellij.lang.javascript.psi;

import consulo.annotation.access.RequiredReadAction;


/**
 * @author VISTALL
 * @since 2019-12-14
 */
public interface JSDestructuringObject extends JSElement {
    @RequiredReadAction
    JSVariable[] getVariables();

    JSDestructuringShorthandedProperty[] getProperties();
}

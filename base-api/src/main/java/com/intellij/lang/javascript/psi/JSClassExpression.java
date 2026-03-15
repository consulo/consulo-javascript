package com.intellij.lang.javascript.psi;

import consulo.annotation.access.RequiredReadAction;


/**
 * @author VISTALL
 * @since 2021-12-11
 */
public interface JSClassExpression extends JSExpression {
    @RequiredReadAction
    JSClass getClassElement();
}

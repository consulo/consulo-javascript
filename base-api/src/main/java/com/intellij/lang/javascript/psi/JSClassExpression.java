package com.intellij.lang.javascript.psi;

import consulo.annotation.access.RequiredReadAction;

import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2021-12-11
 */
public interface JSClassExpression extends JSExpression {
    @Nonnull
    @RequiredReadAction
    JSClass getClassElement();
}

package com.intellij.lang.javascript.psi;

/**
 * @author VISTALL
 * @since 2020-01-01
 */
public interface JSSpreadExpression extends JSExpression {
    JSExpression getInnerExpression();
}

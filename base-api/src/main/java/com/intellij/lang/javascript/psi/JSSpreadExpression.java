package com.intellij.lang.javascript.psi;

/**
 * @author VISTALL
 * @since 2020-01-01
 */
public interface JSSpreadExpression extends JSExpression {
    JSExpression getInnerExpression();

    /**
     * The expression after {@code ...} (IntelliJ's name of {@link #getInnerExpression()}).
     */
    default JSExpression getExpression() {
        return getInnerExpression();
    }
}

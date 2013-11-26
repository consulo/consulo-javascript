package com.sixrr.inspectjs.style;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.psi.tree.IElementType;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NotNull;

public class ChainedEqualityJSInspection extends JavaScriptInspection {

    @NotNull
    public String getID() {
        return "ChainedEqualityComparisonsJS";
    }

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("chained.equality.display.name");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.STYLE_GROUP_NAME;
    }

    @NotNull
    public String buildErrorString(Object... args) {
        return InspectionJSBundle.message("chained.equality.error.string");
    }

    public BaseInspectionVisitor buildVisitor() {
        return new ChainedEqualityVisitor();
    }

    private static class ChainedEqualityVisitor extends BaseInspectionVisitor {

        @Override public void visitJSBinaryExpression(
                @NotNull JSBinaryExpression expression) {
            super.visitJSBinaryExpression(expression);
            if (!(expression.getROperand() != null)) {
                return;
            }
            if (!isEqualityComparison(expression)) {
                return;
            }
            final JSExpression lhs = expression.getLOperand();
            if (!(lhs instanceof JSBinaryExpression)) {
                return;
            }
            if (!isEqualityComparison((JSBinaryExpression) lhs)) {
                return;
            }
            registerError(expression);
        }

        private static boolean isEqualityComparison(
                @NotNull JSBinaryExpression expression) {
            final IElementType tokenType = expression.getOperationSign();
            return JSTokenTypes.EQEQ.equals(tokenType) ||
                    JSTokenTypes.NE.equals(tokenType);
        }
    }
}

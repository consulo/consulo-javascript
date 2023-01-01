package com.sixrr.inspectjs.style;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;

import javax.annotation.Nonnull;

@ExtensionImpl
public class ChainedEqualityJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getID() {
        return "ChainedEqualityComparisonsJS";
    }

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("chained.equality.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.STYLE_GROUP_NAME;
    }

    @Override
	@Nonnull
    public String buildErrorString(Object... args) {
        return InspectionJSBundle.message("chained.equality.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new ChainedEqualityVisitor();
    }

    private static class ChainedEqualityVisitor extends BaseInspectionVisitor {

        @Override public void visitJSBinaryExpression(
                @Nonnull JSBinaryExpression expression) {
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
                @Nonnull JSBinaryExpression expression) {
            final IElementType tokenType = expression.getOperationSign();
            return JSTokenTypes.EQEQ.equals(tokenType) ||
                    JSTokenTypes.NE.equals(tokenType);
        }
    }
}

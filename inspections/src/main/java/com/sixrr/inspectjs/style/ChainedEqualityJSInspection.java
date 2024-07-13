package com.sixrr.inspectjs.style;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class ChainedEqualityJSInspection extends JavaScriptInspection {
    @Override
    public String getID() {
        return "ChainedEqualityComparisonsJS";
    }

    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.chainedEqualityDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.STYLE_GROUP_NAME;
    }

    @RequiredReadAction
    @Override
    @Nonnull
    public String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.chainedEqualityErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new ChainedEqualityVisitor();
    }

    private static class ChainedEqualityVisitor extends BaseInspectionVisitor {
        @Override
        public void visitJSBinaryExpression(@Nonnull JSBinaryExpression expression) {
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

        private static boolean isEqualityComparison(@Nonnull JSBinaryExpression expression) {
            final IElementType tokenType = expression.getOperationSign();
            return JSTokenTypes.EQEQ.equals(tokenType) || JSTokenTypes.NE.equals(tokenType);
        }
    }
}

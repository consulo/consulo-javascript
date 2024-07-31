package com.sixrr.inspectjs.bugs;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;
import jakarta.annotation.Nonnull;
import org.jetbrains.annotations.NonNls;

@ExtensionImpl
public class EqualityComparisonWithCoercionJSInspection extends JavaScriptInspection {
    @Override
    @Nonnull
    public String getID() {
        return "EqualityComparisonWithCoercionJS";
    }

    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.equalityComparisonWithCoercionDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.BUGS_GROUP_NAME.get();
    }

    @RequiredReadAction
    @Override
    @Nonnull
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.equalityComparisonWithCoercionErrorString().get();
    }

    @Override
    protected InspectionJSFix buildFix(PsiElement location, Object state) {
        final JSBinaryExpression expression = (JSBinaryExpression)location;
        final IElementType sign = expression.getOperationSign();
        if (JSTokenTypes.EQEQ == sign) {
            return new EqualityComparisonWithCoercionFix("===");
        }
        else if (JSTokenTypes.NE == sign) {
            return new EqualityComparisonWithCoercionFix("!==");
        }
        return null;
    }

    private static class EqualityComparisonWithCoercionFix extends InspectionJSFix {
        private final String sign;

        public EqualityComparisonWithCoercionFix(String sign) {
            this.sign = sign;
        }

        @Override
        @Nonnull
        public String getName() {
            return InspectionJSLocalize.equalityComparisonWithCoercionFix(sign).get();
        }

        @Override
        protected void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException {
            final JSBinaryExpression expression = (JSBinaryExpression)descriptor.getPsiElement();
            final JSExpression lhs = expression.getLOperand();
            final JSExpression rhs = expression.getROperand();
            replaceExpression(expression, lhs.getText() + sign + rhs.getText());
        }
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new EqualityComparisonWithCoercionVisitor();
    }

    private static class EqualityComparisonWithCoercionVisitor extends BaseInspectionVisitor {
        @Override
        public void visitJSBinaryExpression(@Nonnull JSBinaryExpression expression) {
            super.visitJSBinaryExpression(expression);
            final JSExpression lhs = expression.getLOperand();
            if (lhs == null) {
                return;
            }
            final JSExpression rhs = expression.getROperand();
            if (rhs == null) {
                return;
            }
            final IElementType tokenType = expression.getOperationSign();
            if (!JSTokenTypes.EQEQ.equals(tokenType) && !JSTokenTypes.NE.equals(tokenType)) {
                return;
            }
            if (!mayCauseCoercion(rhs) && !mayCauseCoercion(lhs)) {
                return;
            }
            registerError(expression);
        }
    }

    private static boolean mayCauseCoercion(JSExpression expression) {
        @NonNls final String text = expression.getText();
        return "0".equals(text)
            || "0x0".equals(text)
            || "0X0".equals(text)
            || "0.0".equals(text)
            || "0L".equals(text)
            || "0l".equals(text)
            || "null".equals(text)
            || "undefined".equals(text)
            || "true".equals(text)
            || "false".equals(text)
            || "''".equals(text);
    }
}

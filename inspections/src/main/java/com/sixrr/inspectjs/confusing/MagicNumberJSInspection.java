package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.psi.JSSimpleLiteralExpression;
import consulo.language.psi.PsiElement;
import consulo.language.psi.util.PsiTreeUtil;
import jakarta.annotation.Nonnull;
import org.jetbrains.annotations.NonNls;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

@ExtensionImpl
public class MagicNumberJSInspection extends JavaScriptInspection {
    @NonNls
    private static final String[] s_specialCaseLiteralArray = new String[]{
        "0", "1", "2", "3", "4",
        "5", "6", "7", "8", "9",
        "10", "0L", "1L", "2L", "0l",
        "1l", "2l", "0.0", "1.0", "0.0F",
        "1.0F", "0.0f", "1.0f"
    };

    /**
     * @noinspection StaticCollection
     */
    private static final Set<String> s_specialCaseLiterals = new HashSet<>(23);

    static {
        Collections.addAll(s_specialCaseLiterals, s_specialCaseLiteralArray);
    }

    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.magicNumberDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME.get();
    }

    @RequiredReadAction
    @Override
    @Nonnull
    public String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.magicNumberProblemDescriptor().get();
    }

    static boolean isSpecialCaseLiteral(String text) {
        return s_specialCaseLiterals.contains(text);
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new MagicNumberVisitor();
    }

    private static class MagicNumberVisitor extends BaseInspectionVisitor {
        @Override
        public void visitJSLiteralExpression(@Nonnull JSSimpleLiteralExpression expression) {
            super.visitJSLiteralExpression(expression);

            final String text = expression.getText();
            if (text == null || !isNumeric(text) || isSpecialCaseLiteral(text) || isDeclaredConstant(expression)) {
                return;
            }
            final PsiElement parent = expression.getParent();
            if (parent instanceof JSPrefixExpression) {
                registerError(parent);
            }
            else {
                registerError(expression);
            }
        }
    }

    private static boolean isNumeric(String text) {
        if (text.isEmpty()) {
            return false;
        }
        final char firstChar = text.charAt(0);
        return Character.isDigit(firstChar) || firstChar == '.';
    }

    private static boolean isDeclaredConstant(JSLiteralExpression expression) {
        final JSFunction containingFunction = PsiTreeUtil.getParentOfType(expression, JSFunction.class);
        if (containingFunction != null) {
            return false;
        }
        final JSVarStatement varStatement = PsiTreeUtil.getParentOfType(expression, JSVarStatement.class);
        if (varStatement == null) {
            return false;
        }
        final JSVariable[] variables = varStatement.getVariables();
        for (JSVariable variable : variables) {
            if (expression.equals(variable.getInitializer())) {
                return true;
            }
        }
        return false;
    }
}

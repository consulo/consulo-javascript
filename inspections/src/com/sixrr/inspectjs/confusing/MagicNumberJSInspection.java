package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.psi.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

import java.util.HashSet;
import java.util.Set;

public class MagicNumberJSInspection extends JavaScriptInspection {

    @NonNls private static final String[] s_specialCaseLiteralArray =
            new String[]{
                    "0", "1", "2", "3", "4",
                    "5", "6", "7", "8", "9",
                    "10", "0L", "1L", "2L", "0l",
                    "1l", "2l", "0.0", "1.0", "0.0F",
                    "1.0F", "0.0f", "1.0f"
            };

    /**
     * @noinspection StaticCollection
     */
    private static final Set<String> s_specialCaseLiterals =
            new HashSet<String>(23);

    static {
        for (String string : s_specialCaseLiteralArray) {
            s_specialCaseLiterals.add(string);
        }
    }

    @Override
	@NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("magic.number.display.name");
    }

    @Override
	@NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    @Override
	@NotNull
    public String buildErrorString(Object... args) {
        return InspectionJSBundle.message(
                "magic.number.problem.descriptor");
    }

    static boolean isSpecialCaseLiteral(String text) {
        return s_specialCaseLiterals.contains(text);
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new MagicNumberVisitor();
    }

    private static class MagicNumberVisitor extends BaseInspectionVisitor {

        @Override public void visitJSLiteralExpression(
                @NotNull JSSimpleLiteralExpression expression) {
            super.visitJSLiteralExpression(expression);

            final String text = expression.getText();
            if (text == null) {
                return;
            }
            if(!isNumeric(text))
            {
                return;
            }
            if (isSpecialCaseLiteral(text)) {
                return;
            }
            if (isDeclaredConstant(expression)) {
                return;
            }
            final PsiElement parent = expression.getParent();
            if (parent instanceof JSPrefixExpression) {
                registerError(parent);
            } else {
                registerError(expression);
            }
        }

    }

    private static boolean isNumeric(String text) {
        if(text.length()==0)
        {
            return false;
        }
        final char firstChar = text.charAt(0);
        return Character.isDigit(firstChar)|| firstChar == '.';
    }
    private static boolean isDeclaredConstant(JSLiteralExpression expression) {
        final JSFunction containingFunction =
                PsiTreeUtil.getParentOfType(expression, JSFunction.class);
        if (containingFunction != null) {
            return false;
        }
        final JSVarStatement varStatement =
                PsiTreeUtil.getParentOfType(expression, JSVarStatement.class);
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

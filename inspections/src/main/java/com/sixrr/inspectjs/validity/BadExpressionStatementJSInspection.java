package com.sixrr.inspectjs.validity;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.ecmascript.psi.ES6ExportDefaultAssignment;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.ResolveResult;
import consulo.language.psi.util.PsiTreeUtil;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import org.jetbrains.annotations.NonNls;

@ExtensionImpl
public class BadExpressionStatementJSInspection extends JavaScriptInspection {
    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.expressionStatementWhichIsNotAssignmentOrCallDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.VALIDITY_GROUP_NAME.get();
    }

    @Override
    public boolean isEnabledByDefault() {
        return true;
    }

    @RequiredReadAction
    @Override
    @Nullable
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.expressionStatementIsNotAssignmentOrCallErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        @RequiredReadAction
        public void visitJSExpressionStatement(JSExpressionStatement jsExpressionStatement) {
            super.visitJSExpressionStatement(jsExpressionStatement);
            final JSExpression expression = jsExpressionStatement.getExpression();
            if (isNotPointless(expression) || jsExpressionStatement.getParent() instanceof ES6ExportDefaultAssignment) {
                return;
            }

            PsiFile file = jsExpressionStatement.getContainingFile();
            if (file instanceof JSExpressionCodeFragment) {
                return;
            }

            if (expression instanceof JSReferenceExpression && "debugger".equals(expression.getText())) {
                return;
            }

            registerError(jsExpressionStatement);
        }

        private boolean isNotPointless(final JSExpression expression) {
            if (expression instanceof JSCallExpression) {
                return true;
            }
            if (expression instanceof JSAssignmentExpression) {
                return true;
            }

            if (expression instanceof JSPrefixExpression) {
                final JSPrefixExpression prefix = (JSPrefixExpression)expression;
                final IElementType sign = prefix.getOperationSign();
                if (JSTokenTypes.PLUSPLUS.equals(sign) || JSTokenTypes.MINUSMINUS.equals(sign)) {
                    return true;
                }
                final PsiElement signElement = expression.getFirstChild();
                if (signElement != null) {
                    @NonNls final String text = signElement.getText();
                    if ("delete".equals(text)) {
                        return true;
                    }

                    ASTNode node;
                    if (sign == null && (node = signElement.getNode()) != null && node.getElementType() == JSTokenTypes.VOID_KEYWORD) {
                        if ("void(0)".equals(expression.getText())) {
                            return true;
                        }
                    }
                }
            }
            if (expression instanceof JSPostfixExpression) {
                final JSPostfixExpression prefix = (JSPostfixExpression)expression;
                final IElementType sign = prefix.getOperationSign();
                if (JSTokenTypes.PLUSPLUS.equals(sign) || JSTokenTypes.MINUSMINUS.equals(sign)) {
                    return true;
                }
            }

            if (expression instanceof JSBinaryExpression) {
                final JSBinaryExpression binary = (JSBinaryExpression)expression;
                final IElementType sign = binary.getOperationSign();

                if (sign == JSTokenTypes.ANDAND || sign == JSTokenTypes.OROR) {
                    final JSExpression leftOp = binary.getLOperand();

                    if ((leftOp instanceof JSReferenceExpression || leftOp instanceof JSIndexedPropertyAccessExpression)
                        && isNotPointless(binary.getROperand())) {
                        return true;
                    }
                }
                else if (sign == JSTokenTypes.COMMA) {
                    return isNotPointless(binary.getLOperand()) || isNotPointless(binary.getROperand());
                }
            }

            if (expression instanceof JSParenthesizedExpression parenthesized) {
                return isNotPointless(parenthesized.getInnerExpression());
            }

            if (expression instanceof JSReferenceExpression referenceExpression) {
                if (expression.getParent().getParent() instanceof JSClass
                    || PsiTreeUtil.getParentOfType(expression, PsiFile.class).getContext() != null) {
                    final ResolveResult[] results = referenceExpression.multiResolve(false);
                    if (results.length > 0) {
                        final PsiElement element = results[0].getElement();
                        if (element instanceof JSClass || element instanceof JSFunction) {
                            return true; // class A { import B; B; } // x = "foo"
                        }
                    }
                }
            }

            return false;
        }
    }
}

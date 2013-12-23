package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NotNull;

public class TailRecursionJSInspection extends JavaScriptInspection {

    @Override
	@NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @Override
	@NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message(
                "tail.recursion.display.name");
    }

    @Override
	@NotNull
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message(
                "tail.recursion.problem.descriptor");
    }

    /*
    @Nullable
    protected InspectionJSFix buildFix(PsiElement location) {

        return new RemoveTailRecursionFix();
    }

    private static class RemoveTailRecursionFix extends InspectionJSFix {

        public String getName() {
            return InspectionJSBundle.message(
                    "tail.recursion.replace.quickfix");
        }

        public void doFix(Project project, ProblemDescriptor descriptor)
                throws IncorrectOperationException {

            final PsiElement methodNameToken = descriptor.getPsiElement();
            final JSFunction method =
                    PsiTreeUtil.getParentOfType(methodNameToken,
                            JSFunction.class);
            if (method == null) {
                return;
            }
            final JSSourceElement[] body = method.getBody();
            if (body == null) {
                return;
            }
            final PsiManager psiManager = PsiManager.getInstance(project);
            final CodeStyleManager codeStyleManager =
                    psiManager.getCodeStyleManager();
            final boolean[] containedTailCallInLoop = new boolean[1];
            containedTailCallInLoop[0] = false;
            final StringBuffer buffer = new StringBuffer();
            for (int i = 1; i < body.length; i++) {
                replaceTailCalls(body[i], method, buffer,
                        containedTailCallInLoop);
            }
            final String labelString;
            if (containedTailCallInLoop[0]) {
                labelString = method.getName() + ':';
            } else {
                labelString = "";
            }
            @NonNls final String replacementText = '{' + labelString +
                    "while(true){" +
                    buffer + '}';

            final PsiElementFactory elementFactory =
                    psiManager.getElementFactory();
            final PsiCodeBlock block =
                    elementFactory.createCodeBlockFromText(replacementText,
                            null);
            body.replace(block);
            codeStyleManager.reformat(method);

        }

        private static void replaceTailCalls(
                JSSourceElement element, JSFunction method,
                @NonNls StringBuffer out, boolean[] containedTailCallInLoop) {
            final String text = element.getText();
            if (isTailCallReturn(element, method)) {
                final PsiReturnStatement returnStatement =
                        (PsiReturnStatement) element;
                final PsiMethodCallExpression call = (PsiMethodCallExpression)
                        returnStatement.getReturnValue();
                assert call != null;
                final PsiExpressionList argumentList = call.getArgumentList();
                final PsiExpression[] args = argumentList.getExpressions();
                final JSParameterList parameterList = method
                        .getParameterList();
                final PsiParameter[] parameters =
                        parameterList.getParameters();
                final boolean isInBlock =
                        returnStatement.getParent()instanceof PsiCodeBlock;
                if (!isInBlock) {
                    out.append('{');
                }
                for (int i = 0; i < parameters.length; i++) {
                    final PsiParameter parameter = parameters[i];
                    final PsiExpression arg = args[i];
                    final String parameterName = parameter.getName();
                    final String argText = arg.getText();
                    out.append(parameterName);
                    out.append(" = ");
                    out.append(argText);
                    out.append(';');
                }
                final PsiCodeBlock body = method.getBody();
                assert body != null;
                if (ControlFlowUtils.blockCompletesWithStatement(body,
                        returnStatement)) {
                    //don't do anything, as the continue is unnecessary
                } else if (ControlFlowUtils.isInLoop(element)) {
                    final String methodName = method.getName();
                    containedTailCallInLoop[0] = true;
                    out.append("continue ");
                    out.append(methodName);
                    out.append(';');
                } else {
                    out.append("continue;");
                }
                if (!isInBlock) {
                    out.append('}');
                }
            } else {
                final PsiElement[] children = element.getChildren();
                if (children.length == 0) {
                    out.append(text);
                } else {
                    for (final PsiElement child : children) {
                        replaceTailCalls(child, method, out,
                                containedTailCallInLoop);
                    }
                }
            }
        }

        private static boolean isTailCallReturn(PsiElement element,
                                                JSFunction containingMethod) {
            if (!(element instanceof JSReturnStatement)) {
                return false;
            }
            final JSReturnStatement returnStatement =
                    (JSReturnStatement) element;
            final JSExpression returnValue = returnStatement.getExpression();
            if (!(returnValue instanceof JSCallExpression)) {
                return false;
            }
            final JSCallExpression call =
                    (JSCallExpression) returnValue;
            final PsiMethod method = call.resolveMethod();
            return containingMethod.equals(method);
        }
    }
    */

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new TailRecursionVisitor();
    }

    private static class TailRecursionVisitor extends BaseInspectionVisitor {

        @Override public void visitJSReturnStatement(
                @NotNull JSReturnStatement statement) {
            super.visitJSReturnStatement(statement);
            final JSExpression returnValue = statement.getExpression();
            if (!(returnValue instanceof JSCallExpression)) {
                return;
            }
            final JSFunction containingMethod =
                    PsiTreeUtil.getParentOfType(statement,
                            JSFunction.class);
            if (containingMethod == null) {
                return;
            }
            final JSCallExpression returnCall =
                    (JSCallExpression) returnValue;
            final JSExpression methodExpression = returnCall.getMethodExpression();
            if(!(methodExpression  instanceof JSReferenceExpression))
            {
                return;
            }
            final JSReferenceExpression reference =(JSReferenceExpression) methodExpression;

            final PsiElement referent = reference.resolve();
            if(!(referent instanceof JSFunction))
            {
                return;
            }
            final JSFunction method = (JSFunction) referent;

            if (!method.equals(containingMethod)) {
                return;
            }
            registerFunctionCallError(returnCall);
        }
    }
}

package com.sixrr.inspectjs.style;

import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.lang.javascript.psi.*;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiErrorElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.xml.XmlAttributeValue;
import com.intellij.psi.xml.XmlTagChild;
import com.intellij.util.IncorrectOperationException;
import com.sixrr.inspectjs.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class UnterminatedStatementJSInspection extends JavaScriptInspection {
  @Override
  public boolean isEnabledByDefault() {
    return true;
  }

  private final TerminateStatementFix fix = new TerminateStatementFix();

    @Override
	@NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("unterminated.statement.display.name");
    }

    @Override
	@NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.STYLE_GROUP_NAME;
    }

    @Override
	@Nullable
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("unterminated.statement.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    @Override
	public InspectionJSFix buildFix(PsiElement location) {
        return fix;
    }

    private static class TerminateStatementFix extends InspectionJSFix {
        @Override
		@NotNull
        public String getName() {
            return InspectionJSBundle.message("terminate.statement.fix");
        }

        @Override
		public void doFix(Project project, ProblemDescriptor descriptor)
                throws IncorrectOperationException {
            JSStatement expression = PsiTreeUtil.getParentOfType(descriptor.getPsiElement(), JSStatement.class);
            if (expression instanceof JSBlockStatement && expression.getParent() instanceof JSFunctionExpression) {
              expression = PsiTreeUtil.getParentOfType(expression, JSStatement.class);
            }
            if (expression == null) return;
            final String text = expression.getText();
            replaceStatement(expression, text + ';');
        }
    }

    private static class Visitor extends BaseInspectionVisitor {

        @Override public void visitJSExpressionStatement(JSExpressionStatement statement) {
            super.visitJSExpressionStatement(statement);
            if (statement.getContainingFile() instanceof JSExpressionCodeFragment || isTerminated(statement)) {
                return;
            }
            registerError(statement);
        }
        @Override public void visitJSBreakStatement(JSBreakStatement jsBreakStatement) {
            super.visitJSBreakStatement(jsBreakStatement);
            if (isTerminated(jsBreakStatement)) {
                return;
            }
            registerError(jsBreakStatement);
        }

        @Override public void visitJSContinueStatement(JSContinueStatement jsContinueStatement) {
            super.visitJSContinueStatement(jsContinueStatement);
            if (isTerminated(jsContinueStatement)) {
                return;
            }
            registerError(jsContinueStatement);
        }

        @Override public void visitJSReturnStatement(JSReturnStatement jsReturnStatement) {
            super.visitJSReturnStatement(jsReturnStatement);
            if (isTerminated(jsReturnStatement)) {
                return;
            }
            registerError(jsReturnStatement);
        }

        @Override public void visitJSThrowStatement(JSThrowStatement jsThrowStatement) {
            super.visitJSThrowStatement(jsThrowStatement);
            if (isTerminated(jsThrowStatement)) {
                return;
            }
            registerError(jsThrowStatement);
        }

        @Override public void visitJSDoWhileStatement(JSDoWhileStatement jsDoWhileStatement) {
            super.visitJSDoWhileStatement(jsDoWhileStatement);
            if (isTerminated(jsDoWhileStatement)) {
                return;
            }
            registerError(jsDoWhileStatement);
        }

        @Override public void visitJSVarStatement(JSVarStatement jsVarStatement) {
            super.visitJSVarStatement(jsVarStatement);
            if (isTerminated(jsVarStatement)) {
                return;
            }
            registerError(jsVarStatement);
        }

        @Override
		protected PsiElement getEditorErrorLocation(final PsiElement location) {
          PsiElement editorErrorLocation = PsiTreeUtil.lastChild(location);
          while (editorErrorLocation instanceof PsiErrorElement || (editorErrorLocation != null && editorErrorLocation.getTextLength() == 0)) {
            editorErrorLocation = PsiTreeUtil.prevLeaf(editorErrorLocation);
          }

          return editorErrorLocation;
        }
    }

    private static boolean isTerminated(JSStatement statement) {
        final PsiElement parent = statement.getParent();
        if (parent instanceof JSForInStatement ||
                parent instanceof JSForStatement) {
            return true;
        }
        final String text = statement.getText();
        if (text == null) {
            return true;
        }

        boolean terminated = text.endsWith(";");
        if (!terminated) {
          PsiElement container = PsiTreeUtil.getNonStrictParentOfType(parent, JSFile.class, XmlAttributeValue.class,
                                                                            XmlTagChild.class);
          if (container instanceof JSFile) {
            container = container.getContext();
          }
          terminated = container instanceof XmlAttributeValue; // some inline javascript
        }
        return terminated;
    }
}

package com.sixrr.inspectjs.style;

import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiErrorElement;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;
import consulo.xml.psi.xml.XmlAttributeValue;
import consulo.xml.psi.xml.XmlTagChild;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@ExtensionImpl
public class UnterminatedStatementJSInspection extends JavaScriptInspection
{
	@Override
	public boolean isEnabledByDefault()
	{
		return false;
	}

	private final TerminateStatementFix fix = new TerminateStatementFix();

	@Override
	@Nonnull
	public String getDisplayName()
	{
		return InspectionJSBundle.message("unterminated.statement.display.name");
	}

	@Override
	@Nonnull
	public String getGroupDisplayName()
	{
		return JSGroupNames.STYLE_GROUP_NAME;
	}

	@RequiredReadAction
	@Override
	@Nullable
	protected String buildErrorString(Object state, Object... args)
	{
		return InspectionJSBundle.message("unterminated.statement.error.string");
	}

	@Override
	public BaseInspectionVisitor buildVisitor()
	{
		return new Visitor();
	}

	@Override
	public InspectionJSFix buildFix(PsiElement location, Object state)
	{
		return fix;
	}

	private static class TerminateStatementFix extends InspectionJSFix
	{
		@Override
		@Nonnull
		public String getName()
		{
			return InspectionJSBundle.message("terminate.statement.fix");
		}

		@Override
		public void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException
		{
			JSStatement expression = PsiTreeUtil.getParentOfType(descriptor.getPsiElement(), JSStatement.class);
			if(expression instanceof JSBlockStatement && expression.getParent() instanceof JSFunctionExpression)
			{
				expression = PsiTreeUtil.getParentOfType(expression, JSStatement.class);
			}
			if(expression == null)
			{
				return;
			}
			final String text = expression.getText();
			replaceStatement(expression, text + ';');
		}
	}

	private static class Visitor extends BaseInspectionVisitor
	{

		@Override
		public void visitJSExpressionStatement(JSExpressionStatement statement)
		{
			super.visitJSExpressionStatement(statement);
			if(statement.getContainingFile() instanceof JSExpressionCodeFragment || isTerminated(statement))
			{
				return;
			}
			registerError(statement);
		}

		@Override
		public void visitJSBreakStatement(JSBreakStatement jsBreakStatement)
		{
			super.visitJSBreakStatement(jsBreakStatement);
			if(isTerminated(jsBreakStatement))
			{
				return;
			}
			registerError(jsBreakStatement);
		}

		@Override
		public void visitJSContinueStatement(JSContinueStatement jsContinueStatement)
		{
			super.visitJSContinueStatement(jsContinueStatement);
			if(isTerminated(jsContinueStatement))
			{
				return;
			}
			registerError(jsContinueStatement);
		}

		@Override
		public void visitJSReturnStatement(JSReturnStatement jsReturnStatement)
		{
			super.visitJSReturnStatement(jsReturnStatement);
			if(isTerminated(jsReturnStatement))
			{
				return;
			}
			registerError(jsReturnStatement);
		}

		@Override
		public void visitJSThrowStatement(JSThrowStatement jsThrowStatement)
		{
			super.visitJSThrowStatement(jsThrowStatement);
			if(isTerminated(jsThrowStatement))
			{
				return;
			}
			registerError(jsThrowStatement);
		}

		@Override
		public void visitJSDoWhileStatement(JSDoWhileStatement jsDoWhileStatement)
		{
			super.visitJSDoWhileStatement(jsDoWhileStatement);
			if(isTerminated(jsDoWhileStatement))
			{
				return;
			}
			registerError(jsDoWhileStatement);
		}

		@Override
		public void visitJSVarStatement(JSVarStatement jsVarStatement)
		{
			super.visitJSVarStatement(jsVarStatement);
			if(isTerminated(jsVarStatement))
			{
				return;
			}
			registerError(jsVarStatement);
		}

		@Override
		protected PsiElement getEditorErrorLocation(final PsiElement location)
		{
			PsiElement editorErrorLocation = PsiTreeUtil.lastChild(location);
			while(editorErrorLocation instanceof PsiErrorElement || (editorErrorLocation != null && editorErrorLocation.getTextLength() == 0))
			{
				editorErrorLocation = PsiTreeUtil.prevLeaf(editorErrorLocation);
			}

			return editorErrorLocation;
		}
	}

	private static boolean isTerminated(JSStatement statement)
	{
		final PsiElement parent = statement.getParent();
		if(parent instanceof JSForInStatement || parent instanceof JSForStatement)
		{
			return true;
		}
		final String text = statement.getText();
		if(text == null)
		{
			return true;
		}

		boolean terminated = text.endsWith(";");
		if(!terminated)
		{
			PsiElement container = PsiTreeUtil.getNonStrictParentOfType(parent, JSFile.class, XmlAttributeValue.class, XmlTagChild.class);
			if(container instanceof JSFile)
			{
				container = container.getContext();
			}
			terminated = container instanceof XmlAttributeValue; // some inline javascript
		}
		return terminated;
	}
}

package com.sixrr.inspectjs.validity;

import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.ASTNode;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import consulo.language.psi.PsiFile;
import consulo.language.psi.ResolveResult;
import consulo.language.ast.IElementType;
import consulo.language.psi.util.PsiTreeUtil;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.ecmascript.psi.ES6ExportDefaultAssignment;
import consulo.language.psi.PsiElement;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@ExtensionImpl
public class BadExpressionStatementJSInspection extends JavaScriptInspection
{

	@Override
	@Nonnull
	public String getDisplayName()
	{
		return InspectionJSBundle.message("expression.statement.which.is.not.assignment.or.call.display.name");
	}

	@Override
	@Nonnull
	public String getGroupDisplayName()
	{
		return JSGroupNames.VALIDITY_GROUP_NAME;
	}

	@Override
	public boolean isEnabledByDefault()
	{
		return true;
	}

	@Override
	@Nullable
	protected String buildErrorString(Object... args)
	{
		return InspectionJSBundle.message("expression.statement.is.not.assignment.or.call.error.string");
	}

	@Override
	public BaseInspectionVisitor buildVisitor()
	{
		return new Visitor();
	}

	private static class Visitor extends BaseInspectionVisitor
	{
		@Override
		@RequiredReadAction
		public void visitJSExpressionStatement(JSExpressionStatement jsExpressionStatement)
		{
			super.visitJSExpressionStatement(jsExpressionStatement);
			final JSExpression expression = jsExpressionStatement.getExpression();
			if(isNotPointless(expression) || jsExpressionStatement.getParent() instanceof ES6ExportDefaultAssignment)
			{
				return;
			}

			PsiFile file = jsExpressionStatement.getContainingFile();
			if(file instanceof JSExpressionCodeFragment)
			{
				return;
			}

			if(expression instanceof JSReferenceExpression)
			{
				if("debugger".equals(expression.getText()))
				{
					return;
				}
			}

			registerError(jsExpressionStatement);
		}

		private boolean isNotPointless(final JSExpression expression)
		{
			if(expression instanceof JSCallExpression)
			{
				return true;
			}
			if(expression instanceof JSAssignmentExpression)
			{
				return true;
			}

			if(expression instanceof JSPrefixExpression)
			{
				final JSPrefixExpression prefix = (JSPrefixExpression) expression;
				final IElementType sign = prefix.getOperationSign();
				if(JSTokenTypes.PLUSPLUS.equals(sign) || JSTokenTypes.MINUSMINUS.equals(sign))
				{
					return true;
				}
				final PsiElement signElement = expression.getFirstChild();
				if(signElement != null)
				{
					@NonNls final String text = signElement.getText();
					if("delete".equals(text))
					{
						return true;
					}

					ASTNode node;
					if(sign == null && (node = signElement.getNode()) != null && node.getElementType() == JSTokenTypes.VOID_KEYWORD)
					{
						if("void(0)".equals(expression.getText()))
						{
							return true;
						}
					}
				}
			}
			if(expression instanceof JSPostfixExpression)
			{
				final JSPostfixExpression prefix = (JSPostfixExpression) expression;
				final IElementType sign = prefix.getOperationSign();
				if(JSTokenTypes.PLUSPLUS.equals(sign) || JSTokenTypes.MINUSMINUS.equals(sign))
				{
					return true;
				}
			}

			if(expression instanceof JSBinaryExpression)
			{
				final JSBinaryExpression binary = (JSBinaryExpression) expression;
				final IElementType sign = binary.getOperationSign();

				if(sign == JSTokenTypes.ANDAND || sign == JSTokenTypes.OROR)
				{
					final JSExpression leftOp = binary.getLOperand();

					if((leftOp instanceof JSReferenceExpression || leftOp instanceof JSIndexedPropertyAccessExpression) && isNotPointless(binary.getROperand()))
					{
						return true;
					}
				}
				else if(sign == JSTokenTypes.COMMA)
				{
					return isNotPointless(binary.getLOperand()) || isNotPointless(binary.getROperand());
				}
			}

			if(expression instanceof JSParenthesizedExpression)
			{
				return isNotPointless(((JSParenthesizedExpression) expression).getInnerExpression());
			}

			if(expression instanceof JSReferenceExpression)
			{
				if(expression.getParent().getParent() instanceof JSClass || PsiTreeUtil.getParentOfType(expression, PsiFile.class).getContext() != null)
				{
					final ResolveResult[] results = ((JSReferenceExpression) expression).multiResolve(false);
					if(results.length > 0)
					{
						final PsiElement element = results[0].getElement();
						if(element instanceof JSClass || element instanceof JSFunction)
						{
							return true; // class A { import B; B; } // x = "foo"
						}
					}
				}
			}

			return false;
		}
	}
}

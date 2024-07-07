/*
 * Copyright 2000-2005 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.intellij.lang.javascript.impl.inspections;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.util.JSUtils;
import consulo.annotation.component.ExtensionImpl;
import consulo.document.Document;
import consulo.document.util.TextRange;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.codeStyle.CodeStyleManager;
import consulo.language.editor.FileModificationService;
import consulo.language.editor.inspection.LocalQuickFix;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.editor.inspection.ProblemHighlightType;
import consulo.language.editor.inspection.ProblemsHolder;
import consulo.language.psi.PsiDocumentManager;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.ResolveResult;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;
import org.jetbrains.annotations.NonNls;

import jakarta.annotation.Nonnull;
import java.util.LinkedList;
import java.util.List;

/**
 * @author Maxim.Mossienko
 */
@ExtensionImpl
public class JSUndeclaredVariableInspection extends JSInspection
{
	@NonNls
	public static final String SHORT_NAME = "JSUndeclaredVariable";

	@Override
	@Nonnull
	public String getGroupDisplayName()
	{
		return "General";
	}

	@Override
	@Nonnull
	public String getDisplayName()
	{
		return JavaScriptLocalize.jsUndeclaredVariableInspectionName().get();
	}

	@Override
	@Nonnull
	@NonNls
	public String getShortName()
	{
		return SHORT_NAME;
	}

	@Override
	protected JSElementVisitor createVisitor(final ProblemsHolder holder)
	{
		return new JSElementVisitor()
		{
			@Override
			public void visitJSReferenceExpression(final JSReferenceExpression node)
			{
				final PsiElement parentElement = node.getParent();

				if(!(parentElement instanceof JSCallExpression) && node.shouldCheckReferences() && node.getQualifier() == null && parentElement instanceof
						JSDefinitionExpression)
				{
					final JSSourceElement element = PsiTreeUtil.getParentOfType(node, JSWithStatement.class, JSFunction.class);

					if(!(element instanceof JSWithStatement))
					{
						boolean varReferenceWithoutVar = true;
						final ResolveResult[] resolveResults = node.multiResolve(false);

						for(ResolveResult r : resolveResults)
						{
							final PsiElement resolveResult = r.getElement();
							if(resolveResult instanceof JSVariable ||
									resolveResult instanceof JSFunction)
							{
								varReferenceWithoutVar = false;
								break;
							}
						}

						if(varReferenceWithoutVar)
						{
							final PsiElement nameIdentifier = node.getReferenceNameElement();

							if(nameIdentifier != null)
							{
								final List<LocalQuickFix> fixes = new LinkedList<LocalQuickFix>();

								if(myOnTheFly)
								{
									fixes.add(new DeclareJSVariableIntentionAction(node));
								}

								holder.registerProblem(
									nameIdentifier,
									JavaScriptLocalize.javascriptUndeclaredVariableNameMessage(node.getReferencedName()).get(),
									ProblemHighlightType.GENERIC_ERROR_OR_WARNING,
									!fixes.isEmpty() ? fixes.toArray(new LocalQuickFix[fixes.size()]) : null
								);
							}
						}
					}
				}
				super.visitJSReferenceExpression(node);
			}
		};
	}

	private static boolean isImplicitlyDeclared(final JSReferenceExpression node, final PsiElement parentElement)
	{
		if(parentElement instanceof JSForInStatement)
		{
			final JSExpression varExpression = ((JSForInStatement) parentElement).getVariableExpression();

			return PsiTreeUtil.findCommonParent(varExpression, node) == varExpression;
		}
		else if(parentElement instanceof JSForStatement)
		{
			final JSExpression varExpression = ((JSForStatement) parentElement).getInitialization();

			return PsiTreeUtil.findCommonParent(varExpression, node) == varExpression;
		}
		return false;
	}

	private static class DeclareJSVariableIntentionAction implements LocalQuickFix
	{
		private final JSReferenceExpression myReferenceExpression;
		@NonNls
		private static final String VAR_STATEMENT_START = "var ";
		private final PsiFile myFile;

		DeclareJSVariableIntentionAction(JSReferenceExpression expression)
		{
			myReferenceExpression = expression;
			myFile = expression.getContainingFile();
		}

		@Override
		@Nonnull
		public String getName()
		{
			return JavaScriptLocalize.javascriptDeclareVariableIntentionName(myReferenceExpression.getReferencedName()).get();
		}

		@Override
		@Nonnull
		public String getFamilyName()
		{
			return JavaScriptLocalize.javascriptCreateVariableIntentionFamily().get();
		}

		@Override
		public void applyFix(@Nonnull Project project, @Nonnull ProblemDescriptor descriptor)
		{
			if(!FileModificationService.getInstance().prepareFileForWrite(myFile))
			{
				return;
			}

			PsiElement anchor = JSUtils.findStatementAnchor(myReferenceExpression, myFile);
			boolean implicitlyDeclared = isImplicitlyDeclared(myReferenceExpression, anchor);

			if(implicitlyDeclared)
			{
				anchor = myReferenceExpression;
				final JSStatement statement = PsiTreeUtil.getParentOfType(anchor, JSForStatement.class, JSStatement.class);

				if(statement instanceof JSForStatement)
				{
					final JSExpression initialization = ((JSForStatement) statement).getInitialization();

					if(initialization instanceof JSBinaryExpression && ((JSBinaryExpression) initialization).getOperationSign() == JSTokenTypes.COMMA)
					{
						anchor = ((JSAssignmentExpression) ((JSBinaryExpression) initialization).getLOperand()).getLOperand();
					}
				}
			}

			if(anchor != null)
			{
				boolean anchorChanged = false;

				if(!implicitlyDeclared)
				{
					PsiElement parent = anchor.getParent();

					while(parent instanceof JSBlockStatement || parent instanceof JSIfStatement || parent instanceof JSLoopStatement)
					{
						PsiElement newAnchor = parent.getParent();

						if(newAnchor instanceof JSIfStatement || newAnchor instanceof JSWithStatement || newAnchor instanceof JSLoopStatement ||
								newAnchor instanceof JSTryStatement || newAnchor instanceof JSSwitchStatement)
						{
							anchor = newAnchor;
							parent = anchor.getParent();
							anchorChanged = true;
						}
						else if(newAnchor instanceof JSFile)
						{
							anchor = parent;
							anchorChanged = true;
							break;
						}
						else
						{
							break;
						}
					}
				}

				final TextRange textRange = anchor.getTextRange();
				final int startOffset = textRange.getStartOffset();
				@NonNls StringBuilder builder = new StringBuilder();
				builder.append(VAR_STATEMENT_START);

				if(anchor instanceof JSExpressionStatement)
				{
					JSExpression expr = ((JSExpressionStatement) anchor).getExpression();
					if(expr instanceof JSAssignmentExpression && ((JSAssignmentExpression) expr).getOperationSign() != JSTokenTypes.EQ)
					{
						anchorChanged = true;
					}
				}

				if((anchorChanged || !(anchor instanceof JSExpressionStatement)) && !implicitlyDeclared)
				{
					// var statement should be inserted completely
					builder.append(myReferenceExpression.getReferencedName()).append(";\n");
				}

				Document document = PsiDocumentManager.getInstance(project).getDocument(myFile);
				document.replaceString(startOffset, startOffset, builder);
				PsiDocumentManager.getInstance(project).commitDocument(document);

				try
				{
					CodeStyleManager.getInstance(project).reformatText(myFile, startOffset, textRange.getEndOffset() + builder.length());
				}
				catch(IncorrectOperationException e)
				{
					e.printStackTrace();
				}
				myFile.navigate(true);
			}
		}
	}
}

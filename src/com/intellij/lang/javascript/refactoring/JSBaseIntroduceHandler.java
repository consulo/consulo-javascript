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

package com.intellij.lang.javascript.refactoring;

import java.util.ArrayList;
import java.util.List;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.codeInsight.PsiEquivalenceUtil;
import com.intellij.codeInsight.highlighting.HighlightManager;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.JSLanguageDialect;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.lang.javascript.psi.impl.JSEmbeddedContentImpl;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.util.JSUtils;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.RangeMarker;
import com.intellij.openapi.editor.colors.EditorColors;
import com.intellij.openapi.editor.colors.EditorColorsManager;
import com.intellij.openapi.editor.markup.RangeHighlighter;
import com.intellij.openapi.editor.markup.TextAttributes;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.wm.WindowManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.RefactoringActionHandler;
import com.intellij.refactoring.RefactoringBundle;
import com.intellij.refactoring.util.CommonRefactoringUtil;
import com.intellij.util.IncorrectOperationException;

/**
 * @author ven
 */
public abstract class JSBaseIntroduceHandler<T extends JSElement, S extends BaseIntroduceSettings, D extends JSBaseIntroduceDialog> implements
		RefactoringActionHandler
{
	protected static final Logger LOG = Logger.getInstance("#com.intellij.lang.javascript.refactoring.JSBaseIntroduceHandler");

	protected static JSExpression findExpressionInRange(PsiFile file, int startOffset, int endOffset)
	{
		PsiElement element1 = file.findElementAt(startOffset);
		PsiElement element2 = file.findElementAt(endOffset - 1);
		if(element1 instanceof PsiWhiteSpace)
		{
			startOffset = element1.getTextRange().getEndOffset();
		}
		if(element2 instanceof PsiWhiteSpace)
		{
			endOffset = element2.getTextRange().getStartOffset();
		}

		JSExpression expression = PsiTreeUtil.findElementOfClassAtRange(file, startOffset, endOffset, JSExpression.class);
		int expressionEnd;
		PsiElement sibling;

		if(expression == null || (expressionEnd = expression.getTextRange().getEndOffset()) != endOffset && (expressionEnd != endOffset - 1 ||
				(sibling = expression.getNextSibling()) == null ||
				sibling.getNode().getElementType() != JSTokenTypes.SEMICOLON))
		{
			return null;
		}

		if(expression instanceof JSReferenceExpression && expression.getParent() instanceof JSCallExpression)
		{
			return null;
		}
		if(file.getLanguage() == JavaScriptSupportLoader.JSON)
		{
			expression = null; // there is no vars in json
		}
		return expression;
	}

	protected static JSExpression unparenthesize(JSExpression expression)
	{
		while(expression instanceof JSParenthesizedExpression)
		{
			expression = ((JSParenthesizedExpression) expression).getInnerExpression();
		}

		return expression;
	}

	public static JSExpression[] findExpressionOccurrences(JSElement scope, JSExpression expr)
	{
		List<JSExpression> array = new ArrayList<JSExpression>();
		addExpressionOccurrences(unparenthesize(expr), array, scope);
		return array.toArray(new JSExpression[array.size()]);
	}

	protected static void addExpressionOccurrences(JSExpression expr, List<JSExpression> array, PsiElement scope)
	{
		PsiElement[] children = scope.getChildren();

		for(PsiElement child : children)
		{
			if(child instanceof JSExpression)
			{
				final JSExpression childExpression = unparenthesize((JSExpression) child);

				if(childExpression != null &&
						PsiEquivalenceUtil.areElementsEquivalent(childExpression, expr) &&
						!JSResolveUtil.isSelfReference(scope, child))
				{
					array.add((JSExpression) child);
					continue;
				}
			}
			if(!(child instanceof JSFunction))
			{
				addExpressionOccurrences(expr, array, child);
			}
		}
	}

	@Override
	public void invoke(@NotNull final Project project, final Editor editor, PsiFile file, DataContext dataContext)
	{
		if(!editor.getSelectionModel().hasSelection())
		{
			editor.getSelectionModel().selectLineAtCaret();
		}
		int start = editor.getSelectionModel().getSelectionStart();
		int end = editor.getSelectionModel().getSelectionEnd();

		final JSExpression expression = findIntroducedExpression(file, start, end, editor);
		if(expression == null)
		{
			return;
		}

		if(!CommonRefactoringUtil.checkReadOnlyStatus(project, file))
		{
			return;
		}

		editor.getSelectionModel().removeSelection();
		JSElement scope = findIntroducedScope(expression);
		LOG.assertTrue(scope != null);
		final JSExpression[] occurrences = findExpressionOccurrences(scope, expression);
		final S settings = getSettings(project, editor, expression, occurrences);
		if(settings == null)
		{
			return;
		}

		CommandProcessor.getInstance().executeCommand(project, new Runnable()
		{
			@Override
			public void run()
			{
				ApplicationManager.getApplication().runWriteAction(new Runnable()
				{
					@Override
					public void run()
					{
						doRefactoring(project, editor, new BaseIntroduceContext<S>(expression, occurrences, settings));
					}
				});
			}
		}, getRefactoringName(), null);
	}

	protected static final class BaseIntroduceContext<S>
	{
		public final S settings;
		final JSExpression[] occurences;
		public final JSExpression expression;

		public BaseIntroduceContext(JSExpression _mainoccurence, final JSExpression[] _occurences, S _settings)
		{
			occurences = _occurences;
			expression = _mainoccurence;
			settings = _settings;
		}
	}

	protected JSElement findIntroducedScope(final JSExpression expression)
	{
		return PsiTreeUtil.getParentOfType(expression, JSFunction.class, JSFile.class, JSEmbeddedContentImpl.class);
	}

	protected abstract String getRefactoringName();

	protected abstract String getCannotIntroduceMessagePropertyKey();

	@Nullable
	protected JSExpression findIntroducedExpression(final PsiFile file, final int start, final int end, Editor editor)
	{
		final JSExpression expression = findExpressionInRange(file, start, end);
		if(expression == null)
		{
			CommonRefactoringUtil.showErrorHint(file.getProject(), editor, JSBundle.message(getCannotIntroduceMessagePropertyKey()), getRefactoringName(),
					null);
		}
		return expression;
	}

	@Nullable
	protected S getSettings(Project project, Editor editor, JSExpression expression, final JSExpression[] occurrences)
	{
		ArrayList<RangeHighlighter> highlighters = null;
		if(occurrences.length > 1)
		{
			highlighters = highlightOccurences(project, editor, occurrences);
		}

		final D dialog = createDialog(project, expression, occurrences);
		dialog.show();
		if(highlighters != null)
		{
			for(RangeHighlighter highlighter : highlighters)
			{
				HighlightManager.getInstance(project).removeSegmentHighlighter(editor, highlighter);
			}
		}

		if(dialog.getExitCode() != DialogWrapper.OK_EXIT_CODE)
		{
			return null;
		}

		return createSettings(dialog);
	}

	protected S createSettings(final D dialog)
	{
		return (S) dialog;
	}

	protected abstract D createDialog(final Project project, final JSExpression expression, final JSExpression[] occurrences);

	private void doRefactoring(final Project project, final Editor editor, BaseIntroduceContext<S> introduceContext)
	{
		final S settings = introduceContext.settings;
		JSExpression expression = introduceContext.expression;
		final JSExpression[] occurrences = introduceContext.occurences;

		final boolean replaceAllOccurences = settings.isReplaceAllOccurences();
		@NonNls String varDeclText = getDeclText(settings);
		final PsiFile containingFile = expression.getContainingFile();
		final JSLanguageDialect languageDialect = JSUtils.getDialect(containingFile);
		final boolean ecma = JavaScriptSupportLoader.ECMA_SCRIPT_L4 == languageDialect;
		if(ecma)
		{
			String type = settings.getVariableType();
			if(type == null)
			{
				type = JSResolveUtil.getExpressionType(expression, containingFile);
			}
			varDeclText += ":" + type;
		}

		try
		{
			T anchorStatement = findAnchor(introduceContext, replaceAllOccurences);
			JSVarStatement declaration = prepareDeclaration(varDeclText, introduceContext, project, languageDialect);

			LOG.assertTrue(anchorStatement != null);

			boolean replacedOriginal = false;

			if(anchorStatement == expression.getParent() && anchorStatement instanceof JSExpressionStatement)
			{
				declaration = (JSVarStatement) anchorStatement.replace(declaration);
				editor.getCaretModel().moveToOffset(declaration.getTextRange().getEndOffset());
				replacedOriginal = true;
			}
			else
			{
				JSExpression oldExpression = expression;
				final TextRange expressionTextRange = expression.getTextRange();
				final TextRange statementTextRange = anchorStatement.getTextRange();

				RangeMarker marker = editor.getDocument().createRangeMarker(expressionTextRange);

				// Adding declaration to anchorStatement may invalidate original expression so we need to find it in new tree
				final T jsStatement = addStatementBefore(anchorStatement, declaration);

				if(!expression.isValid())
				{
					final T newAnchorStatement = (T) PsiTreeUtil.getNextSiblingOfType(jsStatement, anchorStatement.getClass());
					final int relativeOffset = marker.getStartOffset() - statementTextRange.getStartOffset();
					JSExpression newExpression = PsiTreeUtil.getParentOfType(newAnchorStatement.findElementAt(relativeOffset), oldExpression.getClass());

					if(newExpression == null)
					{
						assert false : "Could not find " + oldExpression.getClass() + " in " + newAnchorStatement.getText() + " with offset " + marker
								.getStartOffset();
					}

					while(newExpression.getTextRange().getLength() != expressionTextRange.getLength())
					{
						JSExpression candidateExpression = PsiTreeUtil.getParentOfType(newExpression, oldExpression.getClass());
						if(candidateExpression == null)
						{
							break;
						}
						if(candidateExpression.getTextRange().getStartOffset() - newAnchorStatement.getTextRange().getStartOffset() != marker.getStartOffset())
						{
							break;
						}
						newExpression = candidateExpression;
					}

					for(int i = 0; i < occurrences.length; ++i)
					{
						if(occurrences[i] == oldExpression)
						{
							occurrences[i] = newExpression;
							break;
						}
					}

					expression = newExpression;
				}
			}

			final JSExpression refExpr = (JSExpression) JSChangeUtil.createExpressionFromText(project, settings.getVariableName(), languageDialect).getPsi();
			if(replaceAllOccurences)
			{
				List<JSExpression> toHighight = new ArrayList<JSExpression>();
				for(JSExpression occurence : occurrences)
				{
					if(occurence != expression || !replacedOriginal)
					{
						toHighight.add(occurence.replace(refExpr));
					}
					else
					{
						toHighight.add(declaration.getVariables()[0].getInitializer());
					}
				}

				highlightOccurences(project, editor, toHighight.toArray(new JSExpression[toHighight.size()]));
			}
			else if(!replacedOriginal)
			{
				expression.replace(refExpr);
			}
		}
		catch(IncorrectOperationException e)
		{
			LOG.error(e);
		}
	}

	protected JSVarStatement prepareDeclaration(final String varDeclText, BaseIntroduceContext<S> context, final Project project,
			final JSLanguageDialect languageDialect) throws IncorrectOperationException
	{
		JSVarStatement declaration = (JSVarStatement) JSChangeUtil.createStatementFromText(project, varDeclText + " = 0" + JSChangeUtil.getSemicolon
				(project), languageDialect).getPsi();
		declaration.getVariables()[0].getInitializer().replace(context.expression);
		return declaration;
	}

	@NonNls
	protected String getDeclText(S settings)
	{
		return "var " + settings.getVariableName();
	}

	protected T addStatementBefore(final T anchorStatement, final JSVarStatement declaration) throws IncorrectOperationException
	{
		return (T) ((JSStatement) anchorStatement).addStatementBefore(declaration);
	}

	protected T findAnchor(final BaseIntroduceContext<S> context, final boolean replaceAllOccurences)
	{
		JSStatement anchorStatement = replaceAllOccurences ? getAnchorToInsert(context.occurences) : PsiTreeUtil.getParentOfType(context.expression,
				JSStatement.class);
		if(anchorStatement instanceof JSVarStatement &&
				anchorStatement.getParent() instanceof JSStatement &&
				!(anchorStatement.getParent() instanceof JSBlockStatement))
		{
			anchorStatement = (JSStatement) anchorStatement.getParent();
		}
		return (T) anchorStatement;
	}

	private static ArrayList<RangeHighlighter> highlightOccurences(Project project, Editor editor, JSExpression[] occurences)
	{
		HighlightManager highlightManager = HighlightManager.getInstance(project);
		EditorColorsManager colorsManager = EditorColorsManager.getInstance();
		TextAttributes attributes = colorsManager.getGlobalScheme().getAttributes(EditorColors.SEARCH_RESULT_ATTRIBUTES);
		ArrayList<RangeHighlighter> result = new ArrayList<RangeHighlighter>();
		highlightManager.addOccurrenceHighlights(editor, occurences, attributes, true, result);
		WindowManager.getInstance().getStatusBar(project).setInfo(RefactoringBundle.message("press.escape.to.remove.the.highlighting"));
		return result;
	}

	private static JSStatement getAnchorToInsert(final JSExpression[] expressions)
	{
		JSElement place = expressions[0];
		next:
		do
		{
			JSStatement statement = PsiTreeUtil.getParentOfType(place, JSStatement.class); //this is the first expression textually
			LOG.assertTrue(statement != null);

			final PsiElement parent = statement.getParent();
			for(JSExpression expression : expressions)
			{
				if(!PsiTreeUtil.isAncestor(parent, expression, true))
				{
					place = statement;
					continue next;
				}
			}

			return statement;
		}
		while(true);
	}

	protected static JSElement findClassAnchor(final PsiElement expression)
	{
		PsiElement nearestParent = PsiTreeUtil.getParentOfType(expression, JSVarStatement.class, JSFunction.class);
		while(nearestParent != null)
		{
			final PsiElement nextParent = PsiTreeUtil.getParentOfType(nearestParent, JSVarStatement.class, JSFunction.class);
			if(nextParent == null)
			{
				break;
			}
			nearestParent = nextParent;
		}

		if(nearestParent != null)
		{
			return (JSElement) nearestParent;
		}

		JSElement parent = PsiTreeUtil.getParentOfType(expression, JSFile.class, JSClass.class);

		if(parent instanceof JSFile)
		{
			final PsiElement classRef = JSResolveUtil.getClassReferenceForXmlFromContext(parent);
			if(classRef instanceof JSClass)
			{
				parent = (JSElement) classRef;
			}
		}

		return parent;
	}

	protected static JSElement addToClassAnchor(final JSElement anchorStatement, final JSVarStatement declaration) throws IncorrectOperationException
	{
		if(!(anchorStatement instanceof JSClass))
		{
			final JSElement element = findClassAnchor(anchorStatement);
			return (JSElement) element.addBefore(declaration, anchorStatement);
		}
		return (JSElement) anchorStatement.add(declaration);
	}

	@Override
	public void invoke(@NotNull Project project, @NotNull PsiElement[] elements, DataContext dataContext)
	{
		throw new RuntimeException("Not implemented");
	}
}
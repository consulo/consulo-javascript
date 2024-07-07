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

package com.intellij.lang.javascript.impl;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.codeEditor.Editor;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.Language;
import consulo.language.codeStyle.CodeStyleManager;
import consulo.language.editor.action.SmartEnterProcessor;
import consulo.language.psi.*;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.project.Project;

import jakarta.annotation.Nonnull;

/**
 * @author Maxim.Mossienko
 *         Date: Jul 25, 2008
 *         Time: 11:38:15 PM
 */
@ExtensionImpl
public class JSSmartEnterProcessor extends SmartEnterProcessor
{
	@Override
	@RequiredReadAction
	public boolean process(@Nonnull final Project project, @Nonnull final Editor editor, @Nonnull final PsiFile psiFile)
	{
		int offset = editor.getCaretModel().getOffset();
		PsiElement at = psiFile.findElementAt(offset);
		if(at == null && offset > 0)
		{
			at = psiFile.findElementAt(offset - 1);
		}
		if(at == null)
		{
			return false;
		}

		PsiElement element = at instanceof PsiWhiteSpace ? PsiTreeUtil.prevLeaf(at) : at;

		if(element != null && !(element instanceof PsiErrorElement))
		{
			final PsiElement nextMeaningfulElement = evalMeaningfulElement(at, true);
			if(nextMeaningfulElement instanceof PsiErrorElement)
			{
				element = nextMeaningfulElement;
				offset = element.getTextOffset();
			}
		}

		JSStatement statement = PsiTreeUtil.getParentOfType(element, JSStatement.class);

		if(!(element instanceof PsiErrorElement) && statement != null && statement.getLastChild().getNode().getElementType() != JSTokenTypes.SEMICOLON)
		{
			offset = statement.getTextRange().getEndOffset();
			String semicolon = JSChangeUtil.getSemicolon(project);
			int shiftOffset = semicolon.length();

			if(element.getParent() instanceof JSReferenceExpression && PsiTreeUtil.lastChild(statement).getNode().getElementType() != JSTokenTypes.RPAR)
			{
				ResolveResult[] results = ((JSReferenceExpression) element.getParent()).multiResolve(true);

				if(results.length > 0)
				{
					if(results[0].getElement() instanceof JSFunction)
					{
						semicolon = "()" + semicolon;
						shiftOffset = semicolon.length();

						if(((JSFunction) results[0].getElement()).getParameterList().getParameters().length > 0)
						{
							shiftOffset = 1;
						}
					}
				}
			}
			if(!semicolon.isEmpty())
			{
				insertCommitReformat(project, editor, psiFile, offset, semicolon, shiftOffset, false);
				return true;
			}
		}

		final PsiElement prevMeaningfulElement = evalMeaningfulElement(element, false);
		String errorDescription = null;
		if(element != null && !(element instanceof PsiErrorElement) && prevMeaningfulElement != null)
		{
			element = prevMeaningfulElement;
		}

		if (element instanceof PsiErrorElement && (JavaScriptLocalize.javascriptParserMessageExpectedLbrace().get().equals(errorDescription = (
				(PsiErrorElement) element).getErrorDescription()) || JavaScriptLocalize.javascriptParserMessageExpectedStatement().get().equals(errorDescription)))
		{
			String semicolon = "";

			if(element.getParent() instanceof JSFunctionExpression)
			{
				final JSElement base = PsiTreeUtil.getParentOfType(element, JSArgumentList.class, JSIndexedPropertyAccessExpression.class, JSStatement.class);
				if(base instanceof JSStatement && base.getLastChild().getNode().getElementType() != JSTokenTypes.SEMICOLON)
				{
					semicolon = JSChangeUtil.getSemicolon(project);
				}
			}
			insertCommitReformat(project, editor, psiFile, offset, "{\n\n}" + semicolon, 2, true);
			return true;
		}
		else if (JavaScriptLocalize.javascriptParserMessageExpectedLparen().get().equals(errorDescription) 
			|| JavaScriptLocalize.javascriptParserMessageExpectedFunctionName().get().equals(errorDescription))
		{
			insertCommitReformat(project, editor, psiFile, offset, "()", 1, false);
			return true;
		}
		return false;
	}

	@RequiredReadAction
	private void insertCommitReformat(final Project project, final Editor editor, final PsiFile psiFile, final int offset, final String str,
									  final int shiftOffset, boolean adjustLineIndent)
	{
		editor.getDocument().insertString(offset, str);
		editor.getCaretModel().moveToOffset(offset + shiftOffset);
		commit(editor);

		PsiElement at = psiFile.findElementAt(offset + shiftOffset - 1);
		final PsiElement parentOfType = PsiTreeUtil.getParentOfType(at, JSStatement.class, JSFunction.class, JSClass.class, JSFile.class);

		reformat(parentOfType);
		if(adjustLineIndent)
		{
			CodeStyleManager.getInstance(project).adjustLineIndent(psiFile, editor.getCaretModel().getOffset());
		}
	}

	private static PsiElement evalMeaningfulElement(final PsiElement element, boolean forward)
	{
		if(element == null)
		{
			return null;
		}
		PsiElement prev = forward ? PsiTreeUtil.nextLeaf(element) : PsiTreeUtil.prevLeaf(element);
		if(prev == null)
		{
			return null;
		}
		if(prev instanceof PsiWhiteSpace)
		{
			prev = forward ? PsiTreeUtil.nextLeaf(prev) : PsiTreeUtil.prevLeaf(prev);
		}
		return prev;
	}

	@Nonnull
	@Override
	public Language getLanguage()
	{
		return JavaScriptLanguage.INSTANCE;
	}
}

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

package com.intellij.lang.javascript.impl.surroundWith;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import consulo.codeEditor.Editor;
import consulo.document.util.TextRange;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.ast.ASTNode;
import consulo.language.editor.surroundWith.Surrounder;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Created by IntelliJ IDEA.
 * User: yole
 * Date: 12.07.2005
 * Time: 12:47:49
 * To change this template use File | Settings | File Templates.
 */
public class JSWithParenthesesSurrounder implements Surrounder
{
	@Override
	public String getTemplateDescription()
	{
		return JavaScriptLocalize.javascriptSurroundWithParenthesis().get();
	}

	@Override
	public boolean isApplicable(@Nonnull PsiElement[] elements)
	{
		return true;
	}

	@Override
	@Nullable
	public TextRange surroundElements(@Nonnull Project project, @Nonnull Editor editor, @Nonnull PsiElement[] elements) throws
			IncorrectOperationException
	{
		JSExpression expr = (JSExpression) elements[0];
		ASTNode parenthExprNode = JSChangeUtil.createExpressionFromText(project, "(" + expr.getText() + ")").getNode();
		expr.getNode().getTreeParent().replaceChild(expr.getNode(), parenthExprNode);
		int offset = parenthExprNode.getTextRange().getEndOffset();
		return new TextRange(offset, offset);
	}
}

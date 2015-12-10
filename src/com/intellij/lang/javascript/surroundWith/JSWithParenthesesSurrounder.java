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

package com.intellij.lang.javascript.surroundWith;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JavaScriptBundle;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.lang.javascript.psi.util.JSUtils;
import com.intellij.lang.surroundWith.Surrounder;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.util.IncorrectOperationException;

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
		return JavaScriptBundle.message("javascript.surround.with.parenthesis");
	}

	@Override
	public boolean isApplicable(@NotNull PsiElement[] elements)
	{
		return true;
	}

	@Override
	@Nullable
	public TextRange surroundElements(@NotNull Project project, @NotNull Editor editor, @NotNull PsiElement[] elements) throws
			IncorrectOperationException
	{
		JSExpression expr = (JSExpression) elements[0];
		ASTNode parenthExprNode = JSChangeUtil.createExpressionFromText(project, "(" + expr.getText() + ")", JSUtils.getDialect(expr.getContainingFile())).getNode();
		expr.getNode().getTreeParent().replaceChild(expr.getNode(), parenthExprNode);
		int offset = parenthExprNode.getTextRange().getEndOffset();
		return new TextRange(offset, offset);
	}
}

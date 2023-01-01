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

import consulo.document.util.TextRange;
import consulo.language.ast.ASTNode;
import consulo.project.Project;
import consulo.language.psi.PsiElement;

/**
 * Created by IntelliJ IDEA.
 * User: yole
 * Date: 12.07.2005
 * Time: 14:15:25
 * To change this template use File | Settings | File Templates.
 */
public class JSWithBlockSurrounder extends JSStatementSurrounder
{
	@Override
	public String getTemplateDescription()
	{
		return "{ }";
	}

	@Override
	protected String getStatementTemplate(final Project project, PsiElement context)
	{
		return "{ }";
	}

	@Override
	protected ASTNode getInsertBeforeNode(final ASTNode statementNode)
	{
		return statementNode.getLastChildNode();
	}

	@Override
	protected TextRange getSurroundSelectionRange(final ASTNode statementNode)
	{
		int endOffset = statementNode.getTextRange().getEndOffset();
		return new TextRange(endOffset, endOffset);
	}
}

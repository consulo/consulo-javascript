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

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptBundle;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSForStatement;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiWhiteSpace;

/**
 * Created by IntelliJ IDEA.
 * User: yole
 * Date: 12.07.2005
 * Time: 18:29:38
 * To change this template use File | Settings | File Templates.
 */
public class JSWithForSurrounder extends JSStatementSurrounder
{
	@Override
	public String getTemplateDescription()
	{
		return JavaScriptBundle.message("javascript.surround.with.for");
	}

	@Override
	protected String getStatementTemplate(final Project project, PsiElement context)
	{
		return "for(i=0; i<1; i++) { }";
	}

	@Override
	protected ASTNode getInsertBeforeNode(final ASTNode statementNode)
	{
		JSForStatement forStatement = (JSForStatement) statementNode.getPsi();
		return forStatement.getBody().getLastChild().getNode();
	}

	@Override
	protected TextRange getSurroundSelectionRange(final ASTNode statementNode)
	{
		for(ASTNode childNode : statementNode.getChildren(null))
		{
			if(childNode.getElementType() == JSTokenTypes.SEMICOLON ||
					childNode.getPsi() instanceof PsiWhiteSpace ||
					childNode.getPsi() instanceof JSExpression)
			{
				statementNode.removeChild(childNode);
			}
			else if(childNode.getElementType() == JSTokenTypes.RPAR)
			{
				int offset = childNode.getStartOffset();
				return new TextRange(offset, offset);
			}
		}
		return null;
	}
}

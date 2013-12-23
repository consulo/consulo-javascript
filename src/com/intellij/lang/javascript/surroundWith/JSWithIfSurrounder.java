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

import org.jetbrains.annotations.NonNls;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.psi.JSIfStatement;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;

/**
 * Created by IntelliJ IDEA.
 * User: yole
 * Date: 12.07.2005
 * Time: 16:33:55
 * To change this template use File | Settings | File Templates.
 */
public class JSWithIfSurrounder extends JSStatementSurrounder
{
	@Override
	public String getTemplateDescription()
	{
		return JSBundle.message("javascript.surround.with.if");
	}

	@Override
	@NonNls
	protected String getStatementTemplate(final Project project, PsiElement context)
	{
		return "if(a) { }";
	}

	@Override
	protected ASTNode getInsertBeforeNode(final ASTNode statementNode)
	{
		JSIfStatement stmt = (JSIfStatement) statementNode.getPsi();
		return stmt.getThen().getNode().getLastChildNode();
	}

	@Override
	protected TextRange getSurroundSelectionRange(final ASTNode statementNode)
	{
		JSIfStatement stmt = (JSIfStatement) statementNode.getPsi();
		ASTNode conditionNode = stmt.getCondition().getNode();
		int offset = conditionNode.getStartOffset();
		stmt.getNode().removeChild(conditionNode);

		return new TextRange(offset, offset);
	}
}

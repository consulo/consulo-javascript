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
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.psi.JSAssignmentExpression;
import com.intellij.lang.javascript.psi.JSExpressionStatement;
import com.intellij.lang.javascript.psi.JSFunctionExpression;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;

public class JSWithFunctionExpressionSurrounder extends JSStatementSurrounder
{
	public String getTemplateDescription()
	{
		return JSBundle.message("javascript.surround.with.function.expression");
	}

	protected String getStatementTemplate(final Project project, PsiElement context)
	{
		return "aaa = function () { }" + JSChangeUtil.getSemicolon(project);
	}

	protected ASTNode getInsertBeforeNode(final ASTNode statementNode)
	{
		JSFunctionExpression stmt = getFunctionExpr(statementNode);
		return stmt.getBody()[0].getLastChild().getNode();
	}

	private static JSFunctionExpression getFunctionExpr(final ASTNode statementNode)
	{
		return (JSFunctionExpression) ((JSAssignmentExpression) ((JSExpressionStatement) statementNode.getPsi()).getExpression()).getROperand();
	}

	protected TextRange getSurroundSelectionRange(final ASTNode statementNode)
	{
		JSFunctionExpression stmt = getFunctionExpr(statementNode);
		ASTNode conditionNode = stmt.findNameIdentifier();
		int offset = conditionNode.getStartOffset();
		stmt.getParent().getNode().removeChild(conditionNode);

		return new TextRange(offset, offset);
	}
}
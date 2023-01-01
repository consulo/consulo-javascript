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

import consulo.language.ast.ASTNode;
import consulo.javascript.language.JavaScriptBundle;
import com.intellij.lang.javascript.psi.JSFunction;
import consulo.project.Project;
import consulo.document.util.TextRange;
import consulo.language.psi.PsiElement;

public class JSWithFunctionSurrounder extends JSStatementSurrounder
{
	@Override
	public String getTemplateDescription()
	{
		return JavaScriptBundle.message("javascript.surround.with.function");
	}

	@Override
	protected String getStatementTemplate(final Project project, PsiElement context)
	{
		return "function $name$() { }";
	}

	@Override
	protected ASTNode getInsertBeforeNode(final ASTNode statementNode)
	{
		JSFunction stmt = (JSFunction) statementNode.getPsi();
		return stmt.getBody()[0].getLastChild().getNode();
	}

	@Override
	protected TextRange getSurroundSelectionRange(final ASTNode statementNode)
	{
		JSFunction stmt = (JSFunction) statementNode.getPsi();
		ASTNode conditionNode = stmt.getNameIdentifier().getNode();
		int offset = conditionNode.getStartOffset();
		stmt.getNode().removeChild(conditionNode);

		return new TextRange(offset, offset);
	}
}
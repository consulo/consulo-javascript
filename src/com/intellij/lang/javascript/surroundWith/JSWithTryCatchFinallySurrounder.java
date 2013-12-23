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
import org.jetbrains.annotations.NotNull;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.JSCatchBlock;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSTryStatement;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;

/**
 * Created by IntelliJ IDEA.
 * User: yole
 * Date: 12.07.2005
 * Time: 19:21:52
 * To change this template use File | Settings | File Templates.
 */
public class JSWithTryCatchFinallySurrounder extends JSStatementSurrounder
{
	@Override
	public String getTemplateDescription()
	{
		return JSBundle.message("javascript.surround.with.try.catch.finally");
	}

	@Override
	@NonNls
	protected String getStatementTemplate(final Project project, PsiElement context)
	{
		return "try { } catch(e" + getExceptionVarTypeBasedOnContext(context) + ") { } finally { }";
	}

	protected static String getExceptionVarTypeBasedOnContext(@NotNull PsiElement context)
	{
		if(context.getContainingFile().getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4)
		{
			return ":Error";
		}
		return "";
	}

	@Override
	protected ASTNode getInsertBeforeNode(final ASTNode statementNode)
	{
		JSTryStatement stmt = (JSTryStatement) statementNode.getPsi();
		return stmt.getStatement().getLastChild().getNode();
	}

	@Override
	protected TextRange getSurroundSelectionRange(final ASTNode statementNode)
	{
		JSTryStatement stmt = (JSTryStatement) statementNode.getPsi();
		final JSCatchBlock catchBlock = stmt.getCatchBlock();
		if(catchBlock != null)
		{
			int offset = catchBlock.getStatement().getFirstChild().getNode().getStartOffset() + 1;
			return new TextRange(offset, offset);
		}
		final JSStatement finallyStmt = stmt.getFinallyStatement();
		if(finallyStmt != null)
		{
			int offset = finallyStmt.getFirstChild().getNode().getStartOffset() + 1;
			return new TextRange(offset, offset);
		}
		return null;
	}
}

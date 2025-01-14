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

import com.intellij.lang.javascript.psi.JSAssignmentExpression;
import com.intellij.lang.javascript.psi.JSExpressionStatement;
import com.intellij.lang.javascript.psi.JSFunctionExpression;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.document.util.TextRange;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.project.Project;

public class JSWithFunctionExpressionSurrounder extends JSStatementSurrounder {
    @Override
    public String getTemplateDescription() {
        return JavaScriptLocalize.javascriptSurroundWithFunctionExpression().get();
    }

    @Override
    protected String getStatementTemplate(Project project, PsiElement context) {
        return "aaa = function () { }" + JSChangeUtil.getSemicolon(project);
    }

    @Override
    @RequiredReadAction
    protected ASTNode getInsertBeforeNode(ASTNode statementNode) {
        JSFunctionExpression stmt = getFunctionExpr(statementNode);
        return stmt.getBody()[0].getLastChild().getNode();
    }

    @RequiredReadAction
    private static JSFunctionExpression getFunctionExpr(ASTNode statementNode) {
        JSExpressionStatement expression = (JSExpressionStatement)statementNode.getPsi();
        JSAssignmentExpression assignment = (JSAssignmentExpression)expression.getExpression();
        return (JSFunctionExpression)assignment.getROperand();
    }

    @Override
    @RequiredReadAction
    protected TextRange getSurroundSelectionRange(ASTNode statementNode) {
        JSFunctionExpression stmt = getFunctionExpr(statementNode);
        ASTNode conditionNode = stmt.getNameIdentifier().getNode();
        int offset = conditionNode.getStartOffset();
        stmt.getParent().getNode().removeChild(conditionNode);

        return new TextRange(offset, offset);
    }
}
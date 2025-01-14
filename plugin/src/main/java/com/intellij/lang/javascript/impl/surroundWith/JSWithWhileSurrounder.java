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

import com.intellij.lang.javascript.psi.JSWhileStatement;
import consulo.annotation.access.RequiredReadAction;
import consulo.document.util.TextRange;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.project.Project;

/**
 * @author yole
 * @since 2005-07-12
 */
public class JSWithWhileSurrounder extends JSStatementSurrounder {
    @Override
    public String getTemplateDescription() {
        return JavaScriptLocalize.javascriptSurroundWithWhile().get();
    }

    @Override
    protected String getStatementTemplate(Project project, PsiElement context) {
        return "while(true) { }";
    }

    @Override
    @RequiredReadAction
    protected ASTNode getInsertBeforeNode(ASTNode statementNode) {
        JSWhileStatement stmt = (JSWhileStatement)statementNode.getPsi();
        return stmt.getBody().getLastChild().getNode();
    }

    @Override
    @RequiredReadAction
    protected TextRange getSurroundSelectionRange(ASTNode statementNode) {
        JSWhileStatement stmt = (JSWhileStatement)statementNode.getPsi();
        return stmt.getCondition().getTextRange();
    }
}

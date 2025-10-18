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

import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.JSCatchBlock;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSTryStatement;
import consulo.annotation.access.RequiredReadAction;
import consulo.document.util.TextRange;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import jakarta.annotation.Nonnull;

/**
 * @author yole
 * @since 2005-07-12
 */
public class JSWithTryCatchFinallySurrounder extends JSStatementSurrounder {
    @Override
    public LocalizeValue getTemplateDescription() {
        return JavaScriptLocalize.javascriptSurroundWithTryCatchFinally();
    }

    @Override
    @RequiredReadAction
    protected String getStatementTemplate(Project project, PsiElement context) {
        return "try { } catch(e" + getExceptionVarTypeBasedOnContext(context) + ") { } finally { }";
    }

    @RequiredReadAction
    protected static String getExceptionVarTypeBasedOnContext(@Nonnull PsiElement context) {
        if (context.getContainingFile().getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4) {
            return ":Error";
        }
        return "";
    }

    @Override
    @RequiredReadAction
    protected ASTNode getInsertBeforeNode(ASTNode statementNode) {
        JSTryStatement stmt = (JSTryStatement)statementNode.getPsi();
        return stmt.getStatement().getLastChild().getNode();
    }

    @Override
    @RequiredReadAction
    protected TextRange getSurroundSelectionRange(ASTNode statementNode) {
        JSTryStatement stmt = (JSTryStatement)statementNode.getPsi();
        final JSCatchBlock catchBlock = stmt.getCatchBlock();
        if (catchBlock != null) {
            int offset = catchBlock.getStatement().getFirstChild().getNode().getStartOffset() + 1;
            return new TextRange(offset, offset);
        }
        final JSStatement finallyStmt = stmt.getFinallyStatement();
        if (finallyStmt != null) {
            int offset = finallyStmt.getFirstChild().getNode().getStartOffset() + 1;
            return new TextRange(offset, offset);
        }
        return null;
    }
}

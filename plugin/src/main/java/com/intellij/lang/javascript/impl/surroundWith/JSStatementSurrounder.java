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

import jakarta.annotation.Nonnull;

import consulo.codeEditor.Editor;
import consulo.language.codeStyle.CodeStyleManager;
import consulo.language.editor.surroundWith.Surrounder;
import consulo.project.Project;
import consulo.language.util.IncorrectOperationException;
import org.jetbrains.annotations.NonNls;

import jakarta.annotation.Nullable;
import consulo.language.ast.ASTNode;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import consulo.document.util.TextRange;
import consulo.language.psi.PsiElement;

/**
 * @author yole
 * @since 2005-07-12
 */
public abstract class JSStatementSurrounder implements Surrounder {
    @Override
    public boolean isApplicable(@Nonnull PsiElement[] elements) {
        return true;
    }

    @Override
    @Nullable
    public TextRange surroundElements(@Nonnull Project project, @Nonnull Editor editor, @Nonnull PsiElement[] elements)
        throws IncorrectOperationException {
        ASTNode node = JSChangeUtil.createStatementFromText(project, getStatementTemplate(project, elements[0]));

        PsiElement container = elements[0].getParent();
        container.getNode().addChild(node, elements[0].getNode());
        final ASTNode insertBeforeNode = getInsertBeforeNode(node);

        for (int i = 0; i < elements.length; i++) {
            final ASTNode childNode = elements[i].getNode();
            final ASTNode childNodeCopy = childNode.copyElement();

            container.getNode().removeChild(childNode);
            insertBeforeNode.getTreeParent().addChild(childNodeCopy, insertBeforeNode);
        }

        final CodeStyleManager csManager = CodeStyleManager.getInstance(project);
        csManager.reformat(node.getPsi());

        return getSurroundSelectionRange(node);
    }

    @NonNls
    protected abstract String getStatementTemplate(final Project project, PsiElement context);

    protected abstract ASTNode getInsertBeforeNode(final ASTNode statementNode);

    protected abstract TextRange getSurroundSelectionRange(final ASTNode statementNode);
}

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

package com.intellij.lang.javascript.psi.impl;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBlockStatement;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSLabeledStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import consulo.annotation.access.RequiredWriteAction;
import consulo.language.ast.ASTNode;
import consulo.language.codeStyle.CodeStyleManager;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import jakarta.annotation.Nonnull;

/**
 * @author max
 * @since 2005-01-30
 */
public class JSBlockStatementImpl extends JSStatementImpl implements JSBlockStatement {
    public JSBlockStatementImpl(ASTNode node) {
        super(node);
    }

    @Override
    public JSStatement[] getStatements() {
        return findChildrenByClass(JSStatement.class);
    }

    public JSLabeledStatement setLabel(String label) {
        throw new UnsupportedOperationException("TODO: implement");
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSBlock(this);
    }

    @Override
    @RequiredWriteAction
    public PsiElement add(@Nonnull PsiElement element) throws IncorrectOperationException {
        if (element instanceof JSStatement statement) {
            ASTNode node = getNode().findChildByType(JSTokenTypes.RBRACE);
            if (node != null) {
                PsiElement psiElement = super.addAfter(statement, node.getTreePrev().getPsi());
                CodeStyleManager.getInstance(getProject()).reformatNewlyAddedElement(getNode(), psiElement.getNode());
                return psiElement;
            }
        }
        return super.add(element);
    }
}

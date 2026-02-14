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

import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSConditionalExpression;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import consulo.language.ast.IElementType;

import jakarta.annotation.Nonnull;

/**
 * @author max
 * @since 2005-01-30
 */
public class JSConditionalExpressionImpl extends JSExpressionImpl implements JSConditionalExpression {
    public JSConditionalExpressionImpl(ASTNode node) {
        super(node);
    }

    @Override
    @RequiredReadAction
    public JSExpression getCondition() {
        ASTNode child = getNode().getFirstChildNode();
        while (child != null) {
            IElementType type = child.getElementType();
            if (type == JSTokenTypes.QUEST) {
                return null;
            }
            if (JSElementTypes.EXPRESSIONS.contains(type)) {
                return (JSExpression)child.getPsi();
            }
            child = child.getTreeNext();
        }
        return null;
    }

    @Override
    @RequiredReadAction
    public JSExpression getThen() {
        boolean questPassed = false;
        ASTNode child = getNode().getFirstChildNode();
        while (child != null) {
            IElementType type = child.getElementType();
            if (type == JSTokenTypes.QUEST) {
                questPassed = true;
            }
            if (type == JSTokenTypes.COLON) {
                return null;
            }
            if (questPassed && JSElementTypes.EXPRESSIONS.contains(type)) {
                return (JSExpression)child.getPsi();
            }

            child = child.getTreeNext();
        }
        return null;
    }

    @Override
    @RequiredReadAction
    public JSExpression getElse() {
        boolean questPassed = false;
        boolean colonPassed = false;
        ASTNode child = getNode().getFirstChildNode();
        while (child != null) {
            IElementType type = child.getElementType();
            if (type == JSTokenTypes.QUEST) {
                questPassed = true;
            }
            if (type == JSTokenTypes.COLON) {
                colonPassed = true;
            }
            if (questPassed && colonPassed && JSElementTypes.EXPRESSIONS.contains(type)) {
                return (JSExpression)child.getPsi();
            }

            child = child.getTreeNext();
        }
        return null;
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSConditionalExpression(this);
    }
}

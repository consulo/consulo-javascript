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

import consulo.language.ast.ASTNode;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSIndexedPropertyAccessExpression;
import consulo.language.ast.IElementType;
import jakarta.annotation.Nonnull;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 11:59:36 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSIndexedPropertyAccessExpressionImpl extends JSExpressionImpl implements JSIndexedPropertyAccessExpression {
    public JSIndexedPropertyAccessExpressionImpl(final ASTNode node) {
        super(node);
    }

    @Override
    public JSExpression getQualifier() {
        ASTNode child = getNode().getFirstChildNode();
        while (child != null) {
            final IElementType type = child.getElementType();
            if (type == JSTokenTypes.LBRACKET) {
                return null;
            }
            if (child.getPsi() instanceof JSExpression expression) {
                return expression;
            }
            child = child.getTreeNext();
        }
        return null;
    }

    @Override
    public JSExpression getIndexExpression() {
        ASTNode child = getNode().getFirstChildNode();
        boolean bracketPassed = false;
        while (child != null) {
            final IElementType type = child.getElementType();
            if (type == JSTokenTypes.LBRACKET) {
                bracketPassed = true;
            }
            if (bracketPassed && child.getPsi() instanceof JSExpression expression) {
                return expression;
            }
            child = child.getTreeNext();
        }
        return null;
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSIndexedPropertyAccessExpression(this);
    }
}

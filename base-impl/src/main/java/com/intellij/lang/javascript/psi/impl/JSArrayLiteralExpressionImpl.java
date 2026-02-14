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
import com.intellij.lang.javascript.psi.JSArrayLiteralExpression;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;

import jakarta.annotation.Nonnull;

import java.util.ArrayList;
import java.util.List;

/**
 * @author max
 * @since 2005-01-30
 */
public class JSArrayLiteralExpressionImpl extends JSExpressionImpl implements JSArrayLiteralExpression {
    public JSArrayLiteralExpressionImpl(ASTNode node) {
        super(node);
    }

    @Override
    @RequiredReadAction
    public JSExpression[] getExpressions() {
        List<JSExpression> result = new ArrayList<>();
        ASTNode child = getNode().getFirstChildNode();
        boolean wasExpression = false;
        while (child != null) {
            IElementType type = child.getElementType();
            if (child.getPsi() instanceof JSExpression jsExpression) {
                result.add(jsExpression);
                wasExpression = true;
            }
            else if (type == JSTokenTypes.COMMA) {
                if (wasExpression) {
                    wasExpression = false;
                }
                else {
                    result.add(null); // Skipped expression like [a,,b]
                }
            }
            child = child.getTreeNext();
        }

        return result.toArray(new JSExpression[result.size()]);
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSArrayLiteralExpression(this);
    }
}

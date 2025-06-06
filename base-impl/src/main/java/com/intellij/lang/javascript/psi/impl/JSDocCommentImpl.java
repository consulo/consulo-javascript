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

import com.intellij.lang.javascript.JSDocTokenTypes;
import com.intellij.lang.javascript.psi.JSDocComment;
import com.intellij.lang.javascript.psi.JSDocTag;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.IElementType;
import consulo.language.ast.ASTNode;
import jakarta.annotation.Nonnull;

public class JSDocCommentImpl extends JSElementImpl implements JSDocComment {
    public JSDocCommentImpl(ASTNode node) {
        super(node);
    }

    @Override
    @RequiredReadAction
    public IElementType getTokenType() {
        return getNode().getElementType();
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSDocComment(this);
    }

    @Override
    @RequiredReadAction
    public JSDocTag[] getTags() {
        return getFirstChild() instanceof JSDocComment docComment
            ? docComment.getTags()
            : findChildrenByType(JSDocTokenTypes.DOC_TAG, JSDocTag.class);
    }
}
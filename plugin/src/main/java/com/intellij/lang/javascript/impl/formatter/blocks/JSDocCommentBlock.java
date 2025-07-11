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

package com.intellij.lang.javascript.impl.formatter.blocks;

import consulo.document.util.TextRange;
import consulo.language.ast.ASTNode;
import consulo.language.codeStyle.*;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.ArrayList;
import java.util.List;

/**
 * @author yole
 */
public class JSDocCommentBlock implements Block {
    private ASTNode myNode;
    private int myStartOffset;
    private int myEndOffset;
    private Indent myIndent;
    private static final List<Block> EMPTY_BLOCK_LIST = new ArrayList<>();

    public JSDocCommentBlock(ASTNode node, int startOffset, int endOffset, Indent indent) {
        myNode = node;
        myStartOffset = startOffset;
        myEndOffset = endOffset;
        myIndent = indent;
    }

    @Nonnull
    @Override
    public TextRange getTextRange() {
        return new TextRange(myNode.getStartOffset() + myStartOffset, myNode.getStartOffset() + myEndOffset);
    }

    @Nonnull
    @Override
    public List<Block> getSubBlocks() {
        return EMPTY_BLOCK_LIST;
    }

    @Nullable
    @Override
    public Wrap getWrap() {
        return null;
    }

    @Nonnull
    @Override
    public Indent getIndent() {
        return myIndent;
    }

    @Nullable
    @Override
    public Alignment getAlignment() {
        return null;
    }

    @Nullable
    @Override
    public Spacing getSpacing(Block child1, @Nonnull Block child2) {
        return null;
    }

    @Nonnull
    @Override
    public ChildAttributes getChildAttributes(int newChildIndex) {
        return new ChildAttributes(null, null);
    }

    @Override
    public boolean isIncomplete() {
        return false;
    }

    @Override
    public boolean isLeaf() {
        return true;
    }
}

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

package com.intellij.lang.javascript.formatter.blocks;

import java.util.ArrayList;
import java.util.List;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.formatting.Alignment;
import com.intellij.formatting.Block;
import com.intellij.formatting.ChildAttributes;
import com.intellij.formatting.Indent;
import com.intellij.formatting.Spacing;
import com.intellij.formatting.Wrap;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.util.TextRange;

/**
 * @author yole
 */
public class JSDocCommentBlock implements Block
{
	private ASTNode myNode;
	private int myStartOffset;
	private int myEndOffset;
	private Indent myIndent;
	private static final List<Block> EMPTY_BLOCK_LIST = new ArrayList<Block>();

	public JSDocCommentBlock(final ASTNode node, final int startOffset, final int endOffset, final Indent indent)
	{
		myNode = node;
		myStartOffset = startOffset;
		myEndOffset = endOffset;
		myIndent = indent;
	}

	@Override
	@NotNull
	public TextRange getTextRange()
	{
		return new TextRange(myNode.getStartOffset() + myStartOffset, myNode.getStartOffset() + myEndOffset);
	}

	@Override
	@NotNull
	public List<Block> getSubBlocks()
	{
		return EMPTY_BLOCK_LIST;
	}

	@Override
	@Nullable
	public Wrap getWrap()
	{
		return null;
	}

	@Override
	@Nullable
	public Indent getIndent()
	{
		return myIndent;
	}

	@Override
	@Nullable
	public Alignment getAlignment()
	{
		return null;
	}

	@Override
	@Nullable
	public Spacing getSpacing(Block child1, Block child2)
	{
		return null;
	}

	@Override
	@NotNull
	public ChildAttributes getChildAttributes(final int newChildIndex)
	{
		return null;
	}

	@Override
	public boolean isIncomplete()
	{
		return false;
	}

	@Override
	public boolean isLeaf()
	{
		return true;
	}
}

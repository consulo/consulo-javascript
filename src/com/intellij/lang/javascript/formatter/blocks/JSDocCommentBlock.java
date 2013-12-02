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

	@NotNull
	public TextRange getTextRange()
	{
		return new TextRange(myNode.getStartOffset() + myStartOffset, myNode.getStartOffset() + myEndOffset);
	}

	@NotNull
	public List<Block> getSubBlocks()
	{
		return EMPTY_BLOCK_LIST;
	}

	@Nullable
	public Wrap getWrap()
	{
		return null;
	}

	@Nullable
	public Indent getIndent()
	{
		return myIndent;
	}

	@Nullable
	public Alignment getAlignment()
	{
		return null;
	}

	@Nullable
	public Spacing getSpacing(Block child1, Block child2)
	{
		return null;
	}

	@NotNull
	public ChildAttributes getChildAttributes(final int newChildIndex)
	{
		return null;
	}

	public boolean isIncomplete()
	{
		return false;
	}

	public boolean isLeaf()
	{
		return true;
	}
}

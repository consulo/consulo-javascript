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
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.formatter.JSCodeStyleSettings;
import com.intellij.lang.javascript.impl.formatter.JSSpacingProcessor;
import com.intellij.lang.javascript.types.JSFileElementType;
import consulo.language.ast.IElementType;
import consulo.language.codeStyle.*;
import consulo.language.psi.PsiErrorElement;
import consulo.language.psi.PsiWhiteSpace;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.List;

/**
 * @author ven
 */
public class JSBlock implements Block
{
	private ASTNode myNode;

	private final CommonCodeStyleSettings mySettings;

	private Alignment myAlignment;
	private Indent myIndent;
	private Wrap myWrap;
	private List<Block> mySubBlocks = null;

	public JSBlock(final ASTNode node, final Alignment alignment, final Indent indent, final Wrap wrap, final CommonCodeStyleSettings settings)
	{
		myAlignment = alignment;
		myIndent = indent;
		myNode = node;
		myWrap = wrap;
		mySettings = settings;
	}

	public ASTNode getNode()
	{
		return myNode;
	}

	@Override
	@Nonnull
	public TextRange getTextRange()
	{
		return myNode.getTextRange();
	}

	@Override
	@Nonnull
	public List<Block> getSubBlocks()
	{
		if(mySubBlocks == null)
		{
			SubBlockVisitor visitor = new SubBlockVisitor(getSettings());
			visitor.visit(myNode);
			mySubBlocks = visitor.getBlocks();
		}
		return mySubBlocks;
	}

	@Override
	@Nullable
	public Wrap getWrap()
	{
		return myWrap;
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
		return myAlignment;
	}

	@Override
	@Nullable
	public Spacing getSpacing(Block child1, Block child2)
	{
		if(child1 instanceof JSDocCommentBlock || child2 instanceof JSDocCommentBlock || child1 == null)
		{
			return null;
		}
		return new JSSpacingProcessor(getNode(), ((JSBlock) child1).getNode(), ((JSBlock) child2).getNode(), mySettings).getResult();
	}

	@Override
	@Nonnull
	public ChildAttributes getChildAttributes(final int newChildIndex)
	{
		Indent indent = null;
		final IElementType blockElementType = myNode.getElementType();

		if(blockElementType == JSTokenTypes.DOC_COMMENT)
		{
			return new ChildAttributes(Indent.getSpaceIndent(1), null);
		}

		if(blockElementType == JSElementTypes.PACKAGE_STATEMENT)
		{
			final JSCodeStyleSettings customSettings = CodeStyleSettingsManager.getSettings(myNode.getPsi().getProject()).getCustomSettings
					(JSCodeStyleSettings.class);
			if(customSettings.INDENT_PACKAGE_CHILDREN == JSCodeStyleSettings.INDENT)
			{
				indent = Indent.getNormalIndent();
			}
			else
			{
				indent = Indent.getNoneIndent();
			}
		}
		else if(blockElementType == JSElementTypes.BLOCK_STATEMENT ||
				blockElementType == JSElementTypes.CLASS ||
				blockElementType == JSElementTypes.OBJECT_LITERAL_EXPRESSION)
		{
			indent = Indent.getNormalIndent();
		}
		else if(blockElementType instanceof JSFileElementType || blockElementType == JSElementTypes.EMBEDDED_CONTENT)
		{
			indent = Indent.getNoneIndent();
		}
		else if(JSElementTypes.SOURCE_ELEMENTS.contains(blockElementType) ||
				blockElementType == JSElementTypes.FUNCTION_EXPRESSION ||
				blockElementType == JSElementTypes.ATTRIBUTE_LIST)
		{
			indent = Indent.getNoneIndent();
		}

		Alignment alignment = null;
		final List<Block> subBlocks = getSubBlocks();
		for(int i = 0; i < newChildIndex; i++)
		{
			if(i == subBlocks.size())
			{
				break;
			}
			final Alignment childAlignment = subBlocks.get(i).getAlignment();
			if(childAlignment != null)
			{
				alignment = childAlignment;
				break;
			}
		}

		// in for loops, alignment is required only for items within parentheses
		if(blockElementType == JSElementTypes.FOR_STATEMENT || blockElementType == JSElementTypes.FOR_IN_STATEMENT)
		{
			for(int i = 0; i < newChildIndex; i++)
			{
				if(((JSBlock) subBlocks.get(i)).getNode().getElementType() == JSTokenTypes.RPAR)
				{
					alignment = null;
					break;
				}
			}
		}

		return new ChildAttributes(indent, alignment);
	}

	@Override
	public boolean isIncomplete()
	{
		return isIncomplete(myNode);
	}

	private boolean isIncomplete(ASTNode node)
	{
		node.getPsi().getFirstChild(); // expand chameleon
		ASTNode lastChild = node.getLastChildNode();
		while(lastChild != null && lastChild.getPsi() instanceof PsiWhiteSpace)
		{
			lastChild = lastChild.getTreePrev();
		}
		if(lastChild == null)
		{
			return false;
		}
		if(lastChild.getPsi() instanceof PsiErrorElement)
		{
			return true;
		}
		return isIncomplete(lastChild);
	}

	public CommonCodeStyleSettings getSettings()
	{
		return mySettings;
	}

	@Override
	public boolean isLeaf()
	{
		if(myNode.getElementType() == JSTokenTypes.DOC_COMMENT)
		{
			return false;
		}
		return myNode.getFirstChildNode() == null;
	}
}

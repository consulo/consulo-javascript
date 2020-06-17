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

import com.intellij.formatting.*;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSNodeVisitor;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.formatter.JSCodeStyleSettings;
import com.intellij.lang.javascript.formatter.JSSpacingProcessor;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.types.JSFileElementType;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.psi.codeStyle.CodeStyleSettingsManager;
import com.intellij.psi.codeStyle.CommonCodeStyleSettings;
import com.intellij.psi.tree.IElementType;
import consulo.annotation.access.RequiredReadAction;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;

/**
 * @author ven
 */
public class SubBlockVisitor extends JSNodeVisitor
{
	private final List<Block> myBlocks = new ArrayList<>();
	private final CommonCodeStyleSettings mySettings;

	public SubBlockVisitor(CommonCodeStyleSettings settings)
	{
		mySettings = settings;
	}

	public List<Block> getBlocks()
	{
		return myBlocks;
	}

	@Override
	@RequiredReadAction
	public void visitElement(final ASTNode node)
	{
		Alignment alignment = getDefaultAlignment(node);

		PsiElement elt = node.getPsi().getFirstChild(); // expand chameleon

		while(elt != null)
		{
			final ASTNode child = elt.getNode();
			assert child != null;

			if(child.getElementType() != JSTokenTypes.WHITE_SPACE && child.getTextRange().getLength() > 0)
			{
				Wrap wrap = getWrap(node, child);
				Alignment childAlignment = alignmentProjection(alignment, node, child);
				Indent childIndent = getIndent(node, child);
				myBlocks.add(new JSBlock(child, childAlignment, childIndent, wrap, mySettings));
			}

			elt = elt.getNextSibling(); // expand chameleon
		}
	}


	@Override
	public void visitDocComment(final ASTNode node)
	{
		final ASTNode child = node.getFirstChildNode();
		if(child != null && child.getElementType() == JSTokenTypes.DOC_COMMENT)
		{
			visit(child);
		}
		else
		{
			super.visitDocComment(node);
		}
	}

	@Override
	public void visitComment(final ASTNode node)
	{
		buildCommentBlocks(node);
	}

	private void buildCommentBlocks(final ASTNode node)
	{
		String commentText = node.getText();
		int pos = 0;
		while(pos < commentText.length())
		{
			int nextPos = commentText.indexOf('\n', pos);
			if(nextPos < 0)
			{
				nextPos = commentText.length();
			}

			if(pos != nextPos)
			{
				final Indent childIndent = (pos == 0) ? Indent.getNoneIndent() : Indent.getSpaceIndent(1);
				myBlocks.add(new JSDocCommentBlock(node, pos, nextPos, childIndent));
			}
			pos = nextPos + 1;
			while(pos < commentText.length() && (commentText.charAt(pos) == ' ' ||
					commentText.charAt(pos) == '\t' ||
					commentText.charAt(pos) == '\n'))
			{
				pos++;
			}
		}
	}

	@Nullable
	static Alignment getDefaultAlignment(final ASTNode node)
	{
		if(node.getElementType() == JSElementTypes.FOR_STATEMENT ||
				node.getElementType() == JSElementTypes.PARAMETER_LIST ||
				node.getElementType() == JSElementTypes.BINARY_EXPRESSION ||
				node.getElementType() == JSElementTypes.ASSIGNMENT_EXPRESSION ||
				node.getElementType() == JSElementTypes.CONDITIONAL_EXPRESSION)
		{
			return Alignment.createAlignment();
		}

		return null;
	}

	@Nullable
	private Indent getIndent(final ASTNode node, final ASTNode child)
	{
		final IElementType nodeElementType = node.getElementType();

		if(nodeElementType instanceof JSFileElementType || nodeElementType == JSElementTypes.EMBEDDED_CONTENT)
		{
			return Indent.getNoneIndent();
		}

		if(nodeElementType == JSTokenTypes.DOC_COMMENT)
		{
			final ASTNode treePrev = child.getTreePrev();
			if(treePrev != null && treePrev.getPsi() instanceof PsiWhiteSpace && treePrev.getText().indexOf("\n") != -1)
			{
				return Indent.getSpaceIndent(1);
			}
			return Indent.getNoneIndent();
		}

		final IElementType childElementType = child.getElementType();

		if(nodeElementType == JSElementTypes.PACKAGE_STATEMENT && !JSSpacingProcessor.NOT_A_PACKAGE_CONTENT.contains(childElementType))
		{
			final JSCodeStyleSettings customSettings = CodeStyleSettingsManager.getSettings(node.getPsi().getProject()).getCustomSettings(JSCodeStyleSettings
					.class);
			if(customSettings.INDENT_PACKAGE_CHILDREN == JSCodeStyleSettings.INDENT)
			{
				return Indent.getNormalIndent();
			}
			return Indent.getNoneIndent();
		}

		if(nodeElementType == JSElementTypes.ATTRIBUTE_LIST)
		{
			return Indent.getNoneIndent();
		}

		if(childElementType == JSElementTypes.BLOCK_STATEMENT)
		{
			if(nodeElementType == JSElementTypes.FUNCTION_DECLARATION && (mySettings.METHOD_BRACE_STYLE == CodeStyleSettings.NEXT_LINE_SHIFTED || mySettings
					.METHOD_BRACE_STYLE == CodeStyleSettings.NEXT_LINE_SHIFTED2))
			{
				return Indent.getNormalIndent();
			}
			if(mySettings.BRACE_STYLE == CodeStyleSettings.NEXT_LINE_SHIFTED || mySettings.BRACE_STYLE == CodeStyleSettings.NEXT_LINE_SHIFTED2)
			{
				return Indent.getNormalIndent();
			}
			return Indent.getNoneIndent();
		}

		if(childElementType == JSElementTypes.CATCH_BLOCK)
		{
			return Indent.getNoneIndent();
		}

		if(childElementType == JSElementTypes.CASE_CLAUSE)
		{
			return mySettings.INDENT_CASE_FROM_SWITCH ? Indent.getNormalIndent() : Indent.getNoneIndent();
		}

		if(nodeElementType == JSElementTypes.CASE_CLAUSE)
		{
			if(child.getPsi() instanceof JSStatement || JSTokenTypes.COMMENTS.contains(childElementType))
			{
				return Indent.getNormalIndent();
			}
			return Indent.getNoneIndent();
		}

		if(nodeElementType == JSElementTypes.SWITCH_STATEMENT && (childElementType == JSTokenTypes.LBRACE || childElementType == JSTokenTypes.RBRACE))
		{
			return Indent.getNoneIndent();
		}

		if(nodeElementType == JSElementTypes.IF_STATEMENT)
		{
			return Indent.getNoneIndent();
		}

		if(nodeElementType == JSElementTypes.WITH_STATEMENT && JSElementTypes.SOURCE_ELEMENTS.contains(childElementType))
		{
			return Indent.getNormalIndent();
		}

		if(nodeElementType == JSElementTypes.DOWHILE_STATEMENT && childElementType == JSTokenTypes.WHILE_KEYWORD)
		{
			return Indent.getNoneIndent();
		}

		if(nodeElementType == JSElementTypes.TRY_STATEMENT && childElementType == JSTokenTypes.FINALLY_KEYWORD)
		{
			return Indent.getNoneIndent();
		}

		if(nodeElementType == JSElementTypes.BLOCK_STATEMENT ||
				nodeElementType == JSElementTypes.CLASS ||
				nodeElementType == JSElementTypes.PACKAGE_STATEMENT)
		{
			final ASTNode parent = node.getTreeParent();
			if(parent != null && parent.getElementType() == JSElementTypes.FUNCTION_DECLARATION &&
					mySettings.METHOD_BRACE_STYLE == CodeStyleSettings.NEXT_LINE_SHIFTED)
			{
				return Indent.getNoneIndent();
			}
			if(mySettings.BRACE_STYLE == CodeStyleSettings.NEXT_LINE_SHIFTED)
			{
				return Indent.getNoneIndent();
			}
			if(JSElementTypes.SOURCE_ELEMENTS.contains(childElementType) || JSTokenTypes.COMMENTS.contains(childElementType))
			{
				return Indent.getNormalIndent();
			}
			return Indent.getNoneIndent();
		}
		else if(node.getPsi() instanceof JSLoopStatement)
		{
			if(child.getPsi() == ((JSLoopStatement) node.getPsi()).getBody())
			{
				if(childElementType == JSElementTypes.BLOCK_STATEMENT)
				{
					return Indent.getNoneIndent();
				}
				else
				{
					return Indent.getNormalIndent();
				}
			}
		}

		if(JSTokenTypes.COMMENTS.contains(childElementType))
		{
			return Indent.getNormalIndent();
		}
		if(childElementType == JSElementTypes.OBJECT_LITERAL_EXPRESSION)
		{
			return nodeElementType == JSElementTypes.ARRAY_LITERAL_EXPRESSION ? Indent.getNormalIndent() : Indent.getNoneIndent();
		}

		if(nodeElementType == JSElementTypes.XML_LITERAL_EXPRESSION)
		{
			return childElementType == JSElementTypes.XML_LITERAL_EXPRESSION ? Indent.getNormalIndent() : Indent.getNoneIndent();
		}

		if(nodeElementType == JSElementTypes.ARRAY_LITERAL_EXPRESSION)
		{
			if(childElementType == JSTokenTypes.LBRACKET || childElementType == JSTokenTypes.RBRACKET)
			{
				return Indent.getNoneIndent();
			}
			return Indent.getNormalIndent();
		}

		if(nodeElementType == JSElementTypes.EMBEDDED_EXPRESSION)
		{
			if(childElementType == JSTokenTypes.LBRACE || childElementType == JSTokenTypes.RBRACE)
			{
				return Indent.getNoneIndent();
			}
			return Indent.getNormalIndent();
		}

		if(nodeElementType == JSElementTypes.OBJECT_LITERAL_EXPRESSION)
		{
			if(childElementType == JSTokenTypes.LBRACE || childElementType == JSTokenTypes.RBRACE)
			{
				return Indent.getNoneIndent();
			}
			return Indent.getNormalIndent();
		}
		return null;
	}

	@Nullable
	private static Alignment alignmentProjection(final Alignment defaultAlignment, final ASTNode parent, final ASTNode child)
	{
		if(parent.getElementType() == JSElementTypes.FOR_STATEMENT && (JSElementTypes.EXPRESSIONS.contains(child.getElementType()) || child.getElementType
				() == JSElementTypes.VAR_STATEMENT))
		{
			return defaultAlignment;
		}
		else if(parent.getElementType() == JSElementTypes.PARAMETER_LIST && child.getElementType() == JSElementTypes.FORMAL_PARAMETER)
		{
			return defaultAlignment;
		}
		else if(parent.getPsi() instanceof JSBinaryExpression && JSElementTypes.EXPRESSIONS.contains(child.getElementType()))
		{
			return defaultAlignment;
		}
		else if(parent.getElementType() == JSElementTypes.CONDITIONAL_EXPRESSION && JSElementTypes.EXPRESSIONS.contains(child.getElementType()))
		{
			return defaultAlignment;
		}

		return null;
	}

	@Nullable
	private Wrap getWrap(final ASTNode node, final ASTNode child)
	{
		WrapType wrapType = null;
		if(node.getElementType() == JSElementTypes.ASSIGNMENT_EXPRESSION)
		{
			final JSAssignmentExpression assignment = (JSAssignmentExpression) node.getPsi();
			if(child.getElementType() == assignment.getOperationSign() && mySettings.PLACE_ASSIGNMENT_SIGN_ON_NEXT_LINE || child.getPsi() == assignment
					.getROperand() && !mySettings.PLACE_ASSIGNMENT_SIGN_ON_NEXT_LINE)
			{
				wrapType = WrapType.byLegacyRepresentation(mySettings.ASSIGNMENT_WRAP);
			}
		}
		else if(node.getElementType() == JSElementTypes.BINARY_EXPRESSION)
		{
			final JSBinaryExpression binary = (JSBinaryExpression) node.getPsi();
			if(child.getElementType() == binary.getOperationSign() && mySettings.BINARY_OPERATION_SIGN_ON_NEXT_LINE || child.getPsi() == binary.getROperand()
					&& !mySettings.BINARY_OPERATION_SIGN_ON_NEXT_LINE)
			{
				wrapType = WrapType.byLegacyRepresentation(mySettings.BINARY_OPERATION_WRAP);
			}
		}
		else if(node.getElementType() == JSElementTypes.PARENTHESIZED_EXPRESSION)
		{
			if(child == node.findChildByType(JSTokenTypes.LPAR) && mySettings.PARENTHESES_EXPRESSION_LPAREN_WRAP)
			{
				wrapType = WrapType.NORMAL;
			}
			else if(child == node.findChildByType(JSTokenTypes.RPAR) && mySettings.PARENTHESES_EXPRESSION_RPAREN_WRAP)
			{
				wrapType = WrapType.ALWAYS;
			}
		}
		else if(node.getElementType() == JSElementTypes.ARRAY_LITERAL_EXPRESSION)
		{
			if(child == node.findChildByType(JSTokenTypes.LBRACE) && mySettings.ARRAY_INITIALIZER_LBRACE_ON_NEXT_LINE)
			{
				wrapType = WrapType.NORMAL;
			}
			else if(child == node.findChildByType(JSTokenTypes.RPAR) && mySettings.ARRAY_INITIALIZER_RBRACE_ON_NEXT_LINE)
			{
				wrapType = WrapType.ALWAYS;
			}
		}
		else if(node.getElementType() == JSElementTypes.CONDITIONAL_EXPRESSION)
		{
			final IElementType elementType = child.getElementType();
			if((mySettings.TERNARY_OPERATION_SIGNS_ON_NEXT_LINE && (elementType == JSTokenTypes.QUEST || elementType == JSTokenTypes.COLON)) || (!mySettings
					.TERNARY_OPERATION_SIGNS_ON_NEXT_LINE && child.getPsi() instanceof JSExpression))
			{
				wrapType = WrapType.byLegacyRepresentation(mySettings.TERNARY_OPERATION_WRAP);
			}
		}
		else if(node.getElementType() == JSElementTypes.CALL_EXPRESSION)
		{
			if(child == node.findChildByType(JSTokenTypes.LPAR) && mySettings.CALL_PARAMETERS_LPAREN_ON_NEXT_LINE)
			{
				wrapType = WrapType.NORMAL;
			}
			else if(child == node.findChildByType(JSTokenTypes.RPAR) && mySettings.CALL_PARAMETERS_RPAREN_ON_NEXT_LINE)
			{
				wrapType = WrapType.ALWAYS;
			}
		}
		else if(node.getElementType() == JSElementTypes.PARAMETER_LIST)
		{
			if(child.getElementType() == JSElementTypes.FORMAL_PARAMETER)
			{
				wrapType = WrapType.byLegacyRepresentation(mySettings.METHOD_PARAMETERS_WRAP);
			}
		}
		else if(node.getElementType() == JSElementTypes.FOR_STATEMENT || node.getElementType() == JSElementTypes.FOR_IN_STATEMENT)
		{
			if(JSElementTypes.EXPRESSIONS.contains(child.getElementType()))
			{
				wrapType = WrapType.byLegacyRepresentation(mySettings.FOR_STATEMENT_WRAP);
			}
		}

		return wrapType == null ? null : Wrap.createWrap(wrapType, false);
	}
}

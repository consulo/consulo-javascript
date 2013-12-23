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

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.lang.ASTNode;
import com.intellij.lang.LanguageParserDefinitions;
import com.intellij.lang.ParserDefinition;
import com.intellij.lang.javascript.JSLanguageDialect;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.formatter.JSCodeStyleSettings;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.util.JSUtils;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.PsiParserFacade;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.codeStyle.CodeStyleSettingsManager;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.util.IncorrectOperationException;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 31, 2005
 * Time: 7:56:28 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSChangeUtil
{
	@NonNls
	private static final String DUMMY = "dummy.";

	private JSChangeUtil()
	{
	}

	public static ASTNode createNameIdentifier(Project project, String name, IElementType type)
	{
		if(JSTokenTypes.IDENTIFIER_TOKENS_SET.contains(type))
		{
			return createNameIdentifier(project, name);
		}
		else if(type == JSTokenTypes.STRING_LITERAL && !StringUtil.isQuotedString(name))
		{
			return createExpressionFromText(project, "\"" + name + "\"").getFirstChildNode();
		}
		else
		{
			return createExpressionFromText(project, name).getFirstChildNode();
		}
	}

	public static ASTNode createNameIdentifier(Project project, String name)
	{
		final JSExpressionStatement expressionStatement = (JSExpressionStatement) createJSTreeFromTextImpl(project, name + ";", null);
		final JSReferenceExpressionImpl refExpression = (JSReferenceExpressionImpl) expressionStatement.getFirstChild();

		return refExpression.getNode().getFirstChildNode();
	}

	public static ASTNode createExpressionFromText(Project project, @NonNls String text)
	{
		return createExpressionFromText(project, text, null);
	}

	public static ASTNode createExpressionFromText(Project project, @NonNls String text, @Nullable JSLanguageDialect dialect)
	{
		text = "{\n" + text + "\n}";
		PsiElement element = createJSTreeFromTextImpl(project, text, dialect);
		assert element instanceof JSBlockStatement : "\"" + text + "\" was not parsed as BlockStatement";
		element = ((JSBlockStatement) element).getStatements()[0];
		final JSExpressionStatement expressionStatement = (JSExpressionStatement) element;
		final JSExpression expr = (JSExpression) expressionStatement.getFirstChild();
		return expr.getNode();
	}

	public static ASTNode createStatementFromText(Project project, @NonNls String text)
	{
		return createStatementFromText(project, text, null);
	}

	public static ASTNode createStatementFromText(Project project, @NonNls String text, @Nullable JSLanguageDialect dialect)
	{
		final PsiElement element = createJSTreeFromTextImpl(project, text, dialect);
		final JSSourceElement stmt = element instanceof JSSourceElement ? (JSSourceElement) element : null;
		return stmt != null ? stmt.getNode() : null;
	}

	private static
	@Nullable
	PsiElement createJSTreeFromTextImpl(Project project, @NonNls String text, @Nullable JSLanguageDialect dialect)
	{
		ParserDefinition def = dialect != null ? LanguageParserDefinitions.INSTANCE.forLanguage(dialect) : LanguageParserDefinitions.INSTANCE.forLanguage
				(JavaScriptSupportLoader.JAVASCRIPT.getLanguage());
		assert def != null;
		@NonNls String ext = dialect == null ? "js" : dialect.getFileExtension();
		String name = DUMMY + ext;
		final PsiFile dummyFile;
		if(dialect != null && JavaScriptSupportLoader.GWT_DIALECT.equals(dialect))
		{
			dummyFile = PsiFileFactory.getInstance(project).createFileFromText(name, dialect, text);
		}
		else
		{
			dummyFile = PsiFileFactory.getInstance(project).createFileFromText(name, text);
		}

		return dummyFile.getFirstChild();
	}

	public static ASTNode createJSTreeFromText(Project project, @NonNls String text)
	{
		return createJSTreeFromText(project, text, null);
	}

	public static ASTNode createJSTreeFromText(Project project, @NonNls String text, @Nullable JSLanguageDialect languageDialect)
	{
		final PsiElement element = createJSTreeFromTextImpl(project, text, languageDialect);
		if(element != null)
		{
			return element.getNode();
		}
		return null;
	}

	public static JSExpression replaceExpression(JSExpression oldExpr, JSExpression newExpr)
	{
		if(JSUtils.isNeedParenthesis(oldExpr, newExpr))
		{
			ASTNode parenthesized = createExpressionFromText(oldExpr.getProject(), "(a)");
			final JSParenthesizedExpression parenthPsi = (JSParenthesizedExpression) parenthesized.getPsi();
			parenthesized.replaceChild(parenthPsi.getInnerExpression().getNode(), newExpr.getNode().copyElement());
			oldExpr.getParent().getNode().replaceChild(oldExpr.getNode(), parenthesized);
			return parenthPsi;
		}
		else
		{
			final ASTNode newNode = newExpr.getNode().copyElement();
			oldExpr.getParent().getNode().replaceChild(oldExpr.getNode(), newNode);
			return (JSExpression) newNode.getPsi();
		}
	}

	public static JSStatement replaceStatement(JSStatement oldStatement, JSStatement newStatement)
	{
		final ASTNode newNode = newStatement.getNode().copyElement();
		oldStatement.getParent().getNode().replaceChild(oldStatement.getNode(), newNode);
		return (JSStatement) newNode.getPsi();
	}

	public static void doIdentifierReplacement(PsiElement parent, PsiElement identifier, String name)
	{
		final ASTNode nameElement = JSChangeUtil.createNameIdentifier(parent.getProject(), name);
		parent.getNode().replaceChild(identifier.getNode(), nameElement);
	}

	public static PsiElement doAddBefore(final PsiElement jsElement, final PsiElement element,
			final PsiElement anchor) throws IncorrectOperationException
	{
		if(!JSChangeUtil.isStatementOrComment(element) && !(element instanceof PsiWhiteSpace))
		{
			throw new UnsupportedOperationException("js statement or whitespace expected");
		}

		return doDoAddBefore(jsElement, element, anchor);
	}

	public static PsiElement doDoAddBefore(final PsiElement jsElement, final PsiElement element,
			final PsiElement anchor) throws IncorrectOperationException
	{
		final ASTNode elementNode = element.getNode();
		if(elementNode == null)
		{
			throw new IncorrectOperationException("node should not be null");
		}
		ASTNode copiedElementNode = elementNode.copyElement();
		final ASTNode parentNode = jsElement.getNode();
		ASTNode anchorNode = anchor != null ? anchor.getNode() : null;

		anchorNode = insertWhitespaceIfNeeded(anchorNode, elementNode, parentNode, anchorNode);

		parentNode.addChild(copiedElementNode, anchorNode != null ? anchorNode : null);
		if(copiedElementNode.getPsi() instanceof PsiComment && parentNode.getPsi().isPhysical())
		{ // HACK !
			CodeStyleManager.getInstance(element.getProject()).reformatNewlyAddedElement(parentNode, copiedElementNode);
		}

		return copiedElementNode.getPsi();
	}

	private static ASTNode insertWhitespaceIfNeeded(ASTNode anchorNode, final ASTNode elementNode, final ASTNode parentNode,
			final ASTNode insertionPlaceNode) throws IncorrectOperationException
	{
		ParserDefinition parserDef = LanguageParserDefinitions.INSTANCE.forLanguage(parentNode.getPsi().getLanguage());
		final TokenSet comments = parserDef.getCommentTokens(parentNode.getPsi().getLanguage().getVersions()[0]);
		final TokenSet whitespaces = parserDef.getWhitespaceTokens(parentNode.getPsi().getLanguage().getVersions()[0]);

		if(anchorNode != null && ((!whitespaces.contains(anchorNode.getElementType()) && !whitespaces.contains(elementNode.getElementType())) ||
				comments.contains(anchorNode.getElementType()) ||
				comments.contains(elementNode.getElementType()) ||
				elementNode.getPsi() instanceof PsiComment))
		{
			String commentString = " ";
			if(comments.contains(anchorNode.getElementType()) ||
					comments.contains(elementNode.getElementType()) ||
					elementNode.getPsi() instanceof PsiComment)
			{
				commentString = "\n";
			}

			final ASTNode wsNode = PsiParserFacade.SERVICE.getInstance(parentNode.getPsi().getProject()).createWhiteSpaceFromText(commentString).getNode();
			parentNode.addChild(wsNode, insertionPlaceNode);
			anchorNode = wsNode;
		}
		return anchorNode;
	}

	public static boolean isStatementContainer(final PsiElement jsElement)
	{
		return jsElement instanceof JSBlockStatement || jsElement instanceof JSEmbeddedContentImpl || jsElement instanceof JSClass ||
				jsElement instanceof JSPackageStatement;
	}

	public static boolean isStatementOrComment(final PsiElement jsElement)
	{
		return jsElement instanceof JSSourceElement || jsElement instanceof PsiComment;
	}

	public static PsiElement doAddAfter(final PsiElement jsElement, final PsiElement element,
			final PsiElement anchor) throws IncorrectOperationException
	{
		if(!JSChangeUtil.isStatementOrComment(element) && !(element instanceof PsiWhiteSpace))
		{
			throw new UnsupportedOperationException("js statement or whitespace expected");
		}

		return doDoAddAfter(jsElement, element, anchor);
	}

	public static PsiElement doDoAddAfter(final PsiElement jsElement, final PsiElement element,
			final PsiElement anchor) throws IncorrectOperationException
	{
		final ASTNode parentNode = jsElement.getNode();
		final ASTNode node = element.getNode();
		ASTNode anchorNode = anchor != null ? anchor.getNode() : parentNode.getLastChildNode();
		anchorNode = insertWhitespaceIfNeeded(anchorNode, node, parentNode, anchorNode != null ? anchorNode.getTreeNext() : null);

		final ASTNode nodeCopy = node.copyElement();

		if(anchor == null)
		{
			parentNode.addChild(nodeCopy);
		}
		else
		{
			parentNode.addChild(nodeCopy, anchorNode.getTreeNext());
		}

		final ASTNode nextAfter = nodeCopy.getTreeNext();
		insertWhitespaceIfNeeded(nextAfter, node, parentNode, nextAfter);

		return nodeCopy.getPsi();
	}

	public static PsiElement doAddRangeBefore(PsiElement parent, PsiElement first, final PsiElement last,
			final PsiElement anchor) throws IncorrectOperationException
	{
		final PsiElement resultElement;
		PsiElement psiElement = resultElement = doAddBefore(parent, first, anchor);

		while(first != last)
		{
			first = first.getNextSibling();
			if(first == null)
			{
				break;
			}
			psiElement = doAddAfter(parent, first, psiElement);
		}

		return resultElement;
	}

	public static PsiElement doAddRangeAfter(final PsiElement jsElement, PsiElement first, final PsiElement last,
			final PsiElement anchor) throws IncorrectOperationException
	{
		final PsiElement resultElement;
		PsiElement psiElement = resultElement = doAddAfter(jsElement, first, anchor);

		while(first != last)
		{
			first = first.getNextSibling();
			if(first == null)
			{
				break;
			}
			psiElement = doAddAfter(jsElement, first, psiElement);
		}

		return resultElement;
	}

	public static boolean isBlockStatementContainer(final JSElement jsElement)
	{
		return jsElement instanceof JSIfStatement || jsElement instanceof JSLoopStatement;
	}

	public static PsiElement blockDoAddRangeBefore(final PsiElement first, final PsiElement last, final @NotNull PsiElement anchor) throws
			IncorrectOperationException
	{
		BlockAddContext addContext = new BlockAddContext(anchor)
		{
			@Override
			PsiElement doAddElement(PsiElement... element) throws IncorrectOperationException
			{
				return newlyAddedBlock.addRangeBefore(element[0], element[1], codeBlockAnchor);
			}
		};

		return addContext.doAddElement(first, last);
	}

	public static PsiElement blockDoAddRangeAfter(final PsiElement first, final PsiElement last, final @NotNull PsiElement anchor) throws
			IncorrectOperationException
	{
		BlockAddContext addContext = new BlockAddContext(anchor)
		{
			@Override
			PsiElement doAddElement(PsiElement... element) throws IncorrectOperationException
			{
				return newlyAddedBlock.addRangeAfter(element[0], element[1], codeBlockAnchor);
			}
		};

		return addContext.doAddElement(first, last);
	}

	public static PsiElement blockDoAddAfter(final PsiElement element, final @NotNull PsiElement anchor) throws IncorrectOperationException
	{
		BlockAddContext addContext = new BlockAddContext(anchor)
		{
			@Override
			PsiElement doAddElement(PsiElement... element) throws IncorrectOperationException
			{
				return newlyAddedBlock.addAfter(element[0], codeBlockAnchor);
			}
		};

		return addContext.doAddElement(element);
	}

	public static PsiElement blockDoAddBefore(final PsiElement element, final @NotNull PsiElement anchor) throws IncorrectOperationException
	{
		BlockAddContext addContext = new BlockAddContext(anchor)
		{
			@Override
			PsiElement doAddElement(PsiElement... element) throws IncorrectOperationException
			{
				return newlyAddedBlock.addBefore(element[0], codeBlockAnchor);
			}
		};

		return addContext.doAddElement(element);
	}

	public static String getSemicolon(final Project project)
	{
		return CodeStyleSettingsManager.getInstance(project).getCurrentSettings().getCustomSettings(JSCodeStyleSettings.class)
				.USE_SEMICOLON_AFTER_STATEMENT ? ";" : "";
	}

	abstract static class BlockAddContext
	{
		final JSBlockStatement newlyAddedBlock;
		final PsiElement codeBlockAnchor;

		BlockAddContext(final @NotNull PsiElement _anchor) throws IncorrectOperationException
		{
			final ASTNode codeBlockNode = JSChangeUtil.createStatementFromText(_anchor.getProject(), "{ a }");

			newlyAddedBlock = (JSBlockStatement) _anchor.replace(((JSBlockStatement) codeBlockNode.getPsi()));

			final JSStatement artificiallyAddedBlockAnchor = newlyAddedBlock.getStatements()[0];
			codeBlockAnchor = newlyAddedBlock.addBefore(_anchor, artificiallyAddedBlockAnchor);
			artificiallyAddedBlockAnchor.delete();
		}

		abstract PsiElement doAddElement(PsiElement... element) throws IncorrectOperationException;
	}

	static void removeRangeWithRemovalOfCommas(final ASTNode myNode, final ASTNode parent)
	{
		ASTNode from = myNode, to = myNode.getTreeNext();
		boolean seenComma = false;

		if(to != null && to.getElementType() == JSTokenTypes.WHITE_SPACE)
		{
			to = to.getTreeNext();
		}

		if(to != null && to.getElementType() == JSTokenTypes.COMMA)
		{
			to = to.getTreeNext();
			seenComma = true;

			if(to != null && to.getElementType() == JSTokenTypes.WHITE_SPACE)
			{
				to = to.getTreeNext();
			}
		}

		if(!seenComma)
		{
			ASTNode treePrev = from.getTreePrev();

			if(treePrev != null && treePrev.getElementType() == JSTokenTypes.WHITE_SPACE)
			{
				treePrev = treePrev.getTreePrev();
			}
			if(treePrev != null && treePrev.getElementType() == JSTokenTypes.COMMA)
			{
				from = treePrev;
			}

			if(to != null && to.getElementType() == JSTokenTypes.WHITE_SPACE)
			{
				to = to.getTreeNext();
			}
		}
		parent.removeRange(from, to);
	}
}

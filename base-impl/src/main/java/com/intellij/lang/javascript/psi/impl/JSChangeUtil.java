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
import com.intellij.lang.javascript.formatter.JSCodeStyleSettings;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.util.JSUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.javascript.lang.JavaScriptTokenSets;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.ast.TokenSet;
import consulo.language.codeStyle.CodeStyleManager;
import consulo.language.codeStyle.CodeStyleSettingsManager;
import consulo.language.parser.ParserDefinition;
import consulo.language.psi.*;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;
import consulo.util.lang.StringUtil;
import jakarta.annotation.Nullable;
import org.jetbrains.annotations.NonNls;

import jakarta.annotation.Nonnull;

/**
 * @author max
 * @since 2005-01-31
 */
public class JSChangeUtil {
    private JSChangeUtil() {
    }

    @RequiredReadAction
    public static ASTNode createNameIdentifier(Project project, String name, IElementType type) {
        if (JSTokenTypes.IDENTIFIER_TOKENS_SET.contains(type)) {
            return createNameIdentifier(project, name);
        }
        else if (JavaScriptTokenSets.STRING_LITERALS.contains(type) && !StringUtil.isQuotedString(name)) {
            return createExpressionFromText(project, "\"" + name + "\"").getNode().getFirstChildNode();
        }
        else {
            return createExpressionFromText(project, name).getNode().getFirstChildNode();
        }
    }

    @RequiredReadAction
    public static ASTNode createNameIdentifier(Project project, String name) {
        JSExpressionStatement expression = (JSExpressionStatement)createJSTreeFromTextImpl(project, name + ";");
        JSReferenceExpressionImpl refExpr = (JSReferenceExpressionImpl)expression.getFirstChild();

        return refExpr.getNode().getFirstChildNode();
    }

    @Nonnull
    @RequiredReadAction
    public static JSExpression createExpressionFromText(Project project, @NonNls String text) {
        text = "{\n" + text + "\n}";
        PsiElement element = createJSTreeFromTextImpl(project, text);
        assert element instanceof JSBlockStatement : "\"" + text + "\" was not parsed as BlockStatement";
        element = ((JSBlockStatement)element).getStatements()[0];
        JSExpressionStatement expressionStatement = (JSExpressionStatement)element;
        return (JSExpression)expressionStatement.getFirstChild();
    }

    @RequiredReadAction
    public static ASTNode createStatementFromText(Project project, String text) {
        PsiElement element = createJSTreeFromTextImpl(project, text);
        JSSourceElement stmt = element instanceof JSSourceElement sourceElement ? sourceElement : null;
        return stmt != null ? stmt.getNode() : null;
    }

    @Nullable
    @RequiredReadAction
    private static PsiElement createJSTreeFromTextImpl(Project project, String text) {
        PsiFile dummyFile =
            PsiFileFactory.getInstance(project).createFileFromText("dummy.js", JavaScriptLanguage.INSTANCE, text);

        return dummyFile.getFirstChild();
    }

    @RequiredReadAction
    public static ASTNode createJSTreeFromText(Project project, String text) {
        PsiElement element = createJSTreeFromTextImpl(project, text);
        return element != null ? element.getNode() : null;
    }

    @RequiredWriteAction
    public static JSExpression replaceExpression(JSExpression oldExpr, JSExpression newExpr) {
        if (JSUtils.isNeedParenthesis(oldExpr, newExpr)) {
            ASTNode parenthesized = createExpressionFromText(oldExpr.getProject(), "(a)").getNode();
            JSParenthesizedExpression parenthPsi = (JSParenthesizedExpression)parenthesized.getPsi();
            parenthesized.replaceChild(parenthPsi.getInnerExpression().getNode(), newExpr.getNode().copyElement());
            oldExpr.getParent().getNode().replaceChild(oldExpr.getNode(), parenthesized);
            return parenthPsi;
        }
        else {
            ASTNode newNode = newExpr.getNode().copyElement();
            oldExpr.getParent().getNode().replaceChild(oldExpr.getNode(), newNode);
            return (JSExpression)newNode.getPsi();
        }
    }

    @RequiredWriteAction
    public static JSStatement replaceStatement(JSStatement oldStatement, JSStatement newStatement) {
        ASTNode newNode = newStatement.getNode().copyElement();
        oldStatement.getParent().getNode().replaceChild(oldStatement.getNode(), newNode);
        return (JSStatement)newNode.getPsi();
    }

    @RequiredWriteAction
    public static void doIdentifierReplacement(PsiElement parent, PsiElement identifier, String name) {
        ASTNode nameElement = JSChangeUtil.createNameIdentifier(parent.getProject(), name);
        parent.getNode().replaceChild(identifier.getNode(), nameElement);
    }

    @RequiredWriteAction
    public static PsiElement doAddBefore(PsiElement jsElement, PsiElement element, PsiElement anchor)
        throws IncorrectOperationException {
        if (!JSChangeUtil.isStatementOrComment(element) && !(element instanceof PsiWhiteSpace)) {
            throw new UnsupportedOperationException("js statement or whitespace expected");
        }

        return doDoAddBefore(jsElement, element, anchor);
    }

    @RequiredWriteAction
    public static PsiElement doDoAddBefore(PsiElement jsElement, PsiElement element, PsiElement anchor) throws IncorrectOperationException {
        ASTNode elementNode = element.getNode();
        if (elementNode == null) {
            throw new IncorrectOperationException("node should not be null");
        }
        ASTNode copiedElementNode = elementNode.copyElement();
        ASTNode parentNode = jsElement.getNode();
        ASTNode anchorNode = anchor != null ? anchor.getNode() : null;

        anchorNode = insertWhitespaceIfNeeded(anchorNode, elementNode, parentNode, anchorNode);

        parentNode.addChild(copiedElementNode, anchorNode != null ? anchorNode : null);
        if (copiedElementNode.getPsi() instanceof PsiComment comment && parentNode.getPsi().isPhysical()) { // HACK !
            CodeStyleManager.getInstance(element.getProject()).reformatNewlyAddedElement(parentNode, copiedElementNode);
        }

        return copiedElementNode.getPsi();
    }

    @RequiredWriteAction
    private static ASTNode insertWhitespaceIfNeeded(
        ASTNode anchorNode,
        ASTNode elementNode,
        ASTNode parentNode,
        ASTNode insertionPlaceNode
    ) throws IncorrectOperationException {
        ParserDefinition parserDef = ParserDefinition.forLanguage(parentNode.getPsi().getLanguage());
        TokenSet comments = parserDef.getCommentTokens(parentNode.getPsi().getLanguage().getVersions()[0]);
        TokenSet whitespaces = parserDef.getWhitespaceTokens(parentNode.getPsi().getLanguage().getVersions()[0]);

        if (anchorNode != null
            && (!whitespaces.contains(anchorNode.getElementType()) && !whitespaces.contains(elementNode.getElementType())
            || comments.contains(anchorNode.getElementType())
            || comments.contains(elementNode.getElementType())
            || elementNode.getPsi() instanceof PsiComment)) {
            String commentString = " ";
            if (comments.contains(anchorNode.getElementType())
                || comments.contains(elementNode.getElementType())
                || elementNode.getPsi() instanceof PsiComment) {
                commentString = "\n";
            }

            ASTNode wsNode =
                PsiParserFacade.SERVICE.getInstance(parentNode.getPsi().getProject()).createWhiteSpaceFromText(commentString).getNode();
            parentNode.addChild(wsNode, insertionPlaceNode);
            anchorNode = wsNode;
        }
        return anchorNode;
    }

    public static boolean isStatementContainer(PsiElement jsElement) {
        return jsElement instanceof JSBlockStatement
            || jsElement instanceof JSEmbeddedContentImpl
            || jsElement instanceof JSClass
            || jsElement instanceof JSPackageStatement;
    }

    public static boolean isStatementOrComment(PsiElement jsElement) {
        return jsElement instanceof JSSourceElement || jsElement instanceof PsiComment;
    }

    @RequiredWriteAction
    public static PsiElement doAddAfter(PsiElement jsElement, PsiElement element, PsiElement anchor)
        throws IncorrectOperationException {
        if (!JSChangeUtil.isStatementOrComment(element) && !(element instanceof PsiWhiteSpace)) {
            throw new UnsupportedOperationException("js statement or whitespace expected");
        }

        return doDoAddAfter(jsElement, element, anchor);
    }

    @RequiredWriteAction
    public static PsiElement doDoAddAfter(PsiElement jsElement, PsiElement element, PsiElement anchor) throws IncorrectOperationException {
        ASTNode parentNode = jsElement.getNode();
        ASTNode node = element.getNode();
        ASTNode anchorNode = anchor != null ? anchor.getNode() : parentNode.getLastChildNode();
        anchorNode = insertWhitespaceIfNeeded(anchorNode, node, parentNode, anchorNode != null ? anchorNode.getTreeNext() : null);

        ASTNode nodeCopy = node.copyElement();

        if (anchor == null) {
            parentNode.addChild(nodeCopy);
        }
        else {
            parentNode.addChild(nodeCopy, anchorNode.getTreeNext());
        }

        ASTNode nextAfter = nodeCopy.getTreeNext();
        insertWhitespaceIfNeeded(nextAfter, node, parentNode, nextAfter);

        return nodeCopy.getPsi();
    }

    @RequiredWriteAction
    public static PsiElement doAddRangeBefore(PsiElement parent, PsiElement first, PsiElement last, PsiElement anchor)
        throws IncorrectOperationException {
        PsiElement resultElement;
        PsiElement psiElement = resultElement = doAddBefore(parent, first, anchor);

        while (first != last) {
            first = first.getNextSibling();
            if (first == null) {
                break;
            }
            psiElement = doAddAfter(parent, first, psiElement);
        }

        return resultElement;
    }

    @RequiredWriteAction
    public static PsiElement doAddRangeAfter(PsiElement jsElement, PsiElement first, PsiElement last, PsiElement anchor)
        throws IncorrectOperationException {
        PsiElement resultElement;
        PsiElement psiElement = resultElement = doAddAfter(jsElement, first, anchor);

        while (first != last) {
            first = first.getNextSibling();
            if (first == null) {
                break;
            }
            psiElement = doAddAfter(jsElement, first, psiElement);
        }

        return resultElement;
    }

    public static boolean isBlockStatementContainer(JSElement jsElement) {
        return jsElement instanceof JSIfStatement || jsElement instanceof JSLoopStatement;
    }

    @RequiredWriteAction
    public static PsiElement blockDoAddRangeBefore(PsiElement first, PsiElement last, @Nonnull PsiElement anchor)
        throws IncorrectOperationException {
        BlockAddContext addContext = new BlockAddContext(anchor) {
            @Override
            PsiElement doAddElement(PsiElement... element) throws IncorrectOperationException {
                return newlyAddedBlock.addRangeBefore(element[0], element[1], codeBlockAnchor);
            }
        };

        return addContext.doAddElement(first, last);
    }

    @RequiredWriteAction
    public static PsiElement blockDoAddRangeAfter(PsiElement first, PsiElement last, @Nonnull PsiElement anchor)
        throws IncorrectOperationException {
        BlockAddContext addContext = new BlockAddContext(anchor) {
            @Override
            PsiElement doAddElement(PsiElement... element) throws IncorrectOperationException {
                return newlyAddedBlock.addRangeAfter(element[0], element[1], codeBlockAnchor);
            }
        };

        return addContext.doAddElement(first, last);
    }

    @RequiredWriteAction
    public static PsiElement blockDoAddAfter(PsiElement element, @Nonnull PsiElement anchor) throws IncorrectOperationException {
        BlockAddContext addContext = new BlockAddContext(anchor) {
            @Override
            PsiElement doAddElement(PsiElement... element) throws IncorrectOperationException {
                return newlyAddedBlock.addAfter(element[0], codeBlockAnchor);
            }
        };

        return addContext.doAddElement(element);
    }

    @RequiredWriteAction
    public static PsiElement blockDoAddBefore(PsiElement element, @Nonnull PsiElement anchor) throws IncorrectOperationException {
        BlockAddContext addContext = new BlockAddContext(anchor) {
            @Override
            PsiElement doAddElement(PsiElement... element) throws IncorrectOperationException {
                return newlyAddedBlock.addBefore(element[0], codeBlockAnchor);
            }
        };

        return addContext.doAddElement(element);
    }

    public static String getSemicolon(Project project) {
        return CodeStyleSettingsManager.getInstance(project).getCurrentSettings().getCustomSettings(JSCodeStyleSettings.class)
            .USE_SEMICOLON_AFTER_STATEMENT ? ";" : "";
    }

    abstract static class BlockAddContext {
        final JSBlockStatement newlyAddedBlock;
        final PsiElement codeBlockAnchor;

        @RequiredWriteAction
        BlockAddContext(@Nonnull PsiElement _anchor) throws IncorrectOperationException {
            ASTNode codeBlockNode = JSChangeUtil.createStatementFromText(_anchor.getProject(), "{ a }");

            newlyAddedBlock = (JSBlockStatement)_anchor.replace(codeBlockNode.getPsi());

            JSStatement artificiallyAddedBlockAnchor = newlyAddedBlock.getStatements()[0];
            codeBlockAnchor = newlyAddedBlock.addBefore(_anchor, artificiallyAddedBlockAnchor);
            artificiallyAddedBlockAnchor.delete();
        }

        abstract PsiElement doAddElement(PsiElement... element) throws IncorrectOperationException;
    }

    @RequiredWriteAction
    static void removeRangeWithRemovalOfCommas(ASTNode myNode, ASTNode parent) {
        ASTNode from = myNode, to = myNode.getTreeNext();
        boolean seenComma = false;

        if (to != null && to.getElementType() == JSTokenTypes.WHITE_SPACE) {
            to = to.getTreeNext();
        }

        if (to != null && to.getElementType() == JSTokenTypes.COMMA) {
            to = to.getTreeNext();
            seenComma = true;

            if (to != null && to.getElementType() == JSTokenTypes.WHITE_SPACE) {
                to = to.getTreeNext();
            }
        }

        if (!seenComma) {
            ASTNode treePrev = from.getTreePrev();

            if (treePrev != null && treePrev.getElementType() == JSTokenTypes.WHITE_SPACE) {
                treePrev = treePrev.getTreePrev();
            }
            if (treePrev != null && treePrev.getElementType() == JSTokenTypes.COMMA) {
                from = treePrev;
            }

            if (to != null && to.getElementType() == JSTokenTypes.WHITE_SPACE) {
                to = to.getTreeNext();
            }
        }
        parent.removeRange(from, to);
    }
}

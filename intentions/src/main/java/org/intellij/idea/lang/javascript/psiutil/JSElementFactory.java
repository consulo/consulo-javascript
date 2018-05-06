/*
 * Copyright 2005-2006 Olivier Descout
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
package org.intellij.idea.lang.javascript.psiutil;

import javax.annotation.Nonnull;

import org.jetbrains.annotations.NonNls;
import com.intellij.lang.ASTNode;
import com.intellij.lang.LanguageParserDefinitions;
import com.intellij.lang.ParserDefinition;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSBlockStatement;
import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.ReadonlyStatusHandler;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.IncorrectOperationException;
import consulo.javascript.lang.JavaScriptLanguage;

/**
 */
public class JSElementFactory {
    private         static final Class[] WHITESPACE_CLASS       = new Class[] { PsiWhiteSpace.class };

    private JSElementFactory() {}

    public static PsiElement addElementBefore(@Nonnull PsiElement element,
                                              @Nonnull PsiElement newElement) {
        final ASTNode     oldElementParentNode = element.getNode();
        final PsiElement  parentNode           = element.getParent();
        final ASTNode     newElementParentNode = parentNode.getNode();
        final ASTNode     newElementNode       = newElement.getNode();

        if (newElementParentNode == null || newElementNode == null) {
            return null;
        }
        newElementParentNode.addChild(newElementNode, oldElementParentNode);
        return newElement;
    }

    public static PsiElement addElementAfter(@Nonnull PsiElement element,
                                             @Nonnull PsiElement newElement) {
        final ASTNode     elementNode          = element.getNode();
        final ASTNode     oldElementParentNode = ((elementNode == null) ? null : elementNode.getTreeNext());
        final PsiElement  parentNode           = element.getParent();
        final ASTNode     newElementParentNode = parentNode.getNode();
        final ASTNode     newElementNode       = newElement.getNode();

        if (newElementParentNode == null || newElementNode == null) {
            return null;
        }
        newElementParentNode.addChild(newElementNode, oldElementParentNode);

        return newElement;
    }

    public static PsiElement addElementBefore(@Nonnull PsiElement element,
                                              @NonNls @Nonnull String     elementText) {
        final PsiElement newElement = createDummyFile(element.getProject(), elementText).getFirstChild();

        assert (newElement != null);
        return addElementBefore(element, newElement);
    }

    public static PsiElement addElementAfter(@Nonnull PsiElement element,
                                             @Nonnull String     elementText) {
        final PsiElement newElement = createDummyFile(element.getProject(), elementText).getFirstChild();

        assert (newElement != null);
        return addElementAfter(element, newElement);
    }

    public static ASTNode createElementFromText(Project project, String text) {
        final PsiElement element = createDummyFile(project, text).getFirstChild();

        assert (element != null);
        return element.getNode();
    }

    @Nonnull
	private static PsiFile createDummyFile(Project project, String text) {
      final ParserDefinition  def            =
        LanguageParserDefinitions.INSTANCE.forLanguage(JavaScriptLanguage.INSTANCE);

        assert (def != null);
      return PsiFileFactory.getInstance(project)
        .createFileFromText("dummy.js", text);
    }

    public static JSStatement replaceElementWithStatement(@Nonnull JSElement      element,
                                                          @NonNls @Nonnull String statementText)
        throws IncorrectOperationException {
        final ASTNode    newStatementNode       = JSChangeUtil.createStatementFromText(
          element.getProject(),
          statementText);

        final ASTNode    oldStatementParentNode = element.getNode();
        final PsiElement parentNode             = element.getParent();
        final ASTNode    newStatementParentNode = parentNode.getNode();

        if (newStatementParentNode == null || oldStatementParentNode == null || newStatementNode == null) {
            return null;
        }
        newStatementParentNode.replaceChild(oldStatementParentNode, newStatementNode);
        reformat(parentNode);

        return (JSStatement) newStatementNode.getPsi();
    }

    public static JSExpression replaceExpression(@Nonnull JSExpression   expression,
                                                 @NonNls @Nonnull String text)
            throws IncorrectOperationException {
        final JSExpression newExpressionNode = JSChangeUtil.createExpressionFromText(
          expression.getProject(),
          text);

        return replaceExpression(expression, newExpressionNode);
    }

    public static JSExpression replaceExpression(@Nonnull JSExpression expression,
                                                 @Nonnull JSExpression newExpression)
        throws IncorrectOperationException {
        final ASTNode    newExpressionNode = newExpression.getNode();
        final ASTNode    oldExpressionNode = expression.getNode();
        final PsiElement parentNode        = expression.getParent();
        final ASTNode    grandParentNode   = parentNode.getNode();

        if (grandParentNode == null || oldExpressionNode == null || newExpressionNode == null) {
            return null;
        }

        grandParentNode.replaceChild(oldExpressionNode, newExpressionNode);
        reformat(parentNode);

        return (JSExpression) newExpressionNode.getPsi();
    }

    public static JSStatement replaceStatement(@Nonnull JSStatement    statement,
                                               @NonNls @Nonnull String text)
        throws IncorrectOperationException {
        final ASTNode    newStatementNode       = JSChangeUtil.createStatementFromText(
          statement.getProject(),
          text);

        final ASTNode    oldStatementParentNode = statement.getNode();
        final PsiElement parentNode             = statement.getParent();
        final ASTNode    newStatementParentNode = parentNode.getNode();

        if (newStatementParentNode == null || oldStatementParentNode == null || newStatementNode == null) {
            return null;
        }

        newStatementParentNode.replaceChild(oldStatementParentNode, newStatementNode);
        reformat(parentNode);

        return (JSStatement) newStatementNode.getPsi();
    }

    public static JSStatement addStatementBefore(@Nonnull JSStatement    statement,
                                                 @NonNls @Nonnull String previousStatementText)
        throws IncorrectOperationException {
        final ASTNode    newStatementNode       = JSChangeUtil.createStatementFromText(
          statement.getProject(),
          previousStatementText);
        final ASTNode    oldStatementParentNode = statement.getNode();
        final PsiElement parentNode             = statement.getParent();
        final ASTNode    newStatementParentNode = parentNode.getNode();

        if (newStatementParentNode == null || newStatementNode == null) {
            return null;
        }

        newStatementParentNode.addChild(newStatementNode, oldStatementParentNode);
        reformat(parentNode);

        return (JSStatement) newStatementNode.getPsi();
    }

    public static JSStatement addStatementAfter(@Nonnull JSStatement    statement,
                                                @NonNls @Nonnull String nextStatementText)
            throws IncorrectOperationException {
        final ASTNode     newStatementNode       = JSChangeUtil.createStatementFromText(
          statement.getProject(),
          nextStatementText);
        final ASTNode     statementNode          = statement.getNode();
        final ASTNode     oldStatementParentNode = ((statementNode == null) ? null : statementNode.getTreeNext());
        final PsiElement  parentNode             = statement.getParent();
        final ASTNode     newStatementParentNode = parentNode.getNode();

        if (newStatementParentNode == null || newStatementNode == null) {
            return null;
        }

        newStatementParentNode.addChild(newStatementNode, oldStatementParentNode);
        reformat(parentNode);

        return (JSStatement) newStatementNode.getPsi();
    }

    public static void addRangeBefore(JSStatement[] statements, JSStatement statement)
            throws IncorrectOperationException {
        addRangeBefore(statements, 0, statements.length, statement);
    }

    public static void addRangeAfter(JSStatement[] statements, JSStatement statement)
            throws IncorrectOperationException {
        addRangeAfter(statements, 0, statements.length, statement);
    }

    @SuppressWarnings({"ForLoopWithMissingComponent"})
    public static void addRangeBefore(JSStatement[] statements, int start, int length, JSStatement statement)
            throws IncorrectOperationException {
        for (int index = start; index < length; index++) {
            addStatementBefore(statement, statements[index].getText());
        }
    }
    @SuppressWarnings({"ForLoopWithMissingComponent"})
    public static void addRangeAfter(JSStatement[] statements, int start, int length, JSStatement statement)
            throws IncorrectOperationException {
        for (int index = length; --index >= start; ) {
            addStatementAfter(statement, statements[index].getText());
        }
    }

    public static void replaceExpressionWithNegatedExpression(@Nonnull JSExpression newExpression,
                                                              @Nonnull JSExpression exp)
        throws IncorrectOperationException {
        JSExpression expressionToReplace = BoolUtils.findNegation(exp);
        final String replacementString;

        if (expressionToReplace == null) {
            expressionToReplace = exp;

            if (ComparisonUtils.isComparisonOperator(newExpression)) {
                final JSBinaryExpression  binaryExpression  = (JSBinaryExpression) newExpression;
                final IElementType        operationSign     = binaryExpression.getOperationSign();
                final String              negatedComparison = ComparisonUtils.getNegatedOperatorText(operationSign);
                final JSExpression        leftOperand       = binaryExpression.getLOperand();
                final JSExpression        rightOperand      = binaryExpression.getROperand();

                assert (rightOperand != null);

                replacementString = leftOperand.getText() + negatedComparison + rightOperand.getText();
            } else {
                replacementString = '!' + ParenthesesUtils.getParenthesized(newExpression, ParenthesesUtils.PREFIX_PRECENDENCE);
            }
        } else {
            replacementString = newExpression.getText();
        }
        replaceExpression(expressionToReplace, replacementString);
    }

    public static void replaceExpressionWithNegatedExpressionString(JSExpression exp, String newExpression)
        throws IncorrectOperationException {
        assert (exp != null);

        JSExpression expressionToReplace = BoolUtils.findNegation(exp);
        String       replacementString   = newExpression;

        if (expressionToReplace == null) {
            expressionToReplace = exp;
            replacementString   = "!(" + newExpression + ')';
        }

        replaceExpression(expressionToReplace, replacementString);
    }


    public static void replaceStatementWithUnwrapping(JSStatement statement, JSStatement newBranch)
            throws IncorrectOperationException {
        if (!(newBranch instanceof JSBlockStatement)) {
            JSElementFactory.replaceStatement(statement, newBranch.getText());
            return;
        }

        final JSBlockStatement parentBlock = PsiTreeUtil.getParentOfType(newBranch, JSBlockStatement.class);

        if (parentBlock == null) {
            JSElementFactory.replaceStatement(statement, newBranch.getText());
            return;
        }

        final JSBlockStatement block = (JSBlockStatement) newBranch;

        if (ControlFlowUtils.containsConflictingDeclarations(block, parentBlock)) {
            JSElementFactory.replaceStatement(statement, newBranch.getText());
            return;
        }

        final PsiElement containingElement = statement.getParent();

        assert (containingElement instanceof JSStatement);

        JSElementFactory.addRangeBefore(block.getStatements(), statement);
        JSElementFactory.removeElement(statement);
    }

    public static void removeElement(PsiElement element) {
        final ASTNode node       = element.getNode();
        final ASTNode parentNode = element.getParent().getNode();

        if (node != null && parentNode != null) {
            parentNode.removeChild(node);
        }
    }

    public static void reformat(PsiElement statement) throws IncorrectOperationException {
        // Reformat only in .js files due to a bug in JavaScript reformatting module when doing it in JSP files
        if (statement.getContainingFile() instanceof JSFile) {
			CodeStyleManager.getInstance(statement.getProject()).reformat(statement);
        }
    }

    public static PsiElement getNonWhiteSpaceSibling(PsiElement element, boolean forward) {
        return forward
               ? PsiTreeUtil.skipSiblingsForward(element, WHITESPACE_CLASS)
               : PsiTreeUtil.skipSiblingsBackward(element, WHITESPACE_CLASS);
    }

    public static boolean isFileReadOnly(Project project, PsiFile file) {
        final ReadonlyStatusHandler instance     = ReadonlyStatusHandler.getInstance(project);
        final VirtualFile virtualFile = file.getVirtualFile();

        return virtualFile != null && instance.ensureFilesWritable(virtualFile).hasReadonlyFiles();
    }
}

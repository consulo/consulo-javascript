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

import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.codeStyle.CodeStyleManager;
import consulo.language.parser.ParserDefinition;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiFileFactory;
import consulo.language.psi.PsiWhiteSpace;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;
import consulo.virtualFileSystem.ReadonlyStatusHandler;
import consulo.virtualFileSystem.VirtualFile;
import org.jetbrains.annotations.NonNls;

import jakarta.annotation.Nonnull;

/**
 *
 */
public class JSElementFactory {
    private static final Class[] WHITESPACE_CLASS = new Class[]{PsiWhiteSpace.class};

    private JSElementFactory() {
    }

    public static PsiElement addElementBefore(@Nonnull PsiElement element, @Nonnull PsiElement newElement) {
        ASTNode oldElementParentNode = element.getNode();
        PsiElement parentNode = element.getParent();
        ASTNode newElementParentNode = parentNode.getNode();
        ASTNode newElementNode = newElement.getNode();

        if (newElementParentNode == null || newElementNode == null) {
            return null;
        }
        newElementParentNode.addChild(newElementNode, oldElementParentNode);
        return newElement;
    }

    public static PsiElement addElementAfter(@Nonnull PsiElement element, @Nonnull PsiElement newElement) {
        ASTNode elementNode = element.getNode();
        ASTNode oldElementParentNode = ((elementNode == null) ? null : elementNode.getTreeNext());
        PsiElement parentNode = element.getParent();
        ASTNode newElementParentNode = parentNode.getNode();
        ASTNode newElementNode = newElement.getNode();

        if (newElementParentNode == null || newElementNode == null) {
            return null;
        }
        newElementParentNode.addChild(newElementNode, oldElementParentNode);

        return newElement;
    }

    public static PsiElement addElementBefore(@Nonnull PsiElement element, @NonNls @Nonnull String elementText) {
        PsiElement newElement = createDummyFile(element.getProject(), elementText).getFirstChild();

        assert (newElement != null);
        return addElementBefore(element, newElement);
    }

    public static PsiElement addElementAfter(@Nonnull PsiElement element, @Nonnull String elementText) {
        PsiElement newElement = createDummyFile(element.getProject(), elementText).getFirstChild();

        assert (newElement != null);
        return addElementAfter(element, newElement);
    }

    public static ASTNode createElementFromText(Project project, String text) {
        PsiElement element = createDummyFile(project, text).getFirstChild();

        assert (element != null);
        return element.getNode();
    }

    @Nonnull
    private static PsiFile createDummyFile(Project project, String text) {
        ParserDefinition def = ParserDefinition.forLanguage(JavaScriptLanguage.INSTANCE);

        assert (def != null);
        return PsiFileFactory.getInstance(project)
            .createFileFromText("dummy.js", text);
    }

    public static JSStatement replaceElementWithStatement(@Nonnull JSElement element, @NonNls @Nonnull String statementText)
        throws IncorrectOperationException {
        ASTNode newStatementNode = JSChangeUtil.createStatementFromText(
            element.getProject(),
            statementText
        );

        ASTNode oldStatementParentNode = element.getNode();
        PsiElement parentNode = element.getParent();
        ASTNode newStatementParentNode = parentNode.getNode();

        if (newStatementParentNode == null || oldStatementParentNode == null || newStatementNode == null) {
            return null;
        }
        newStatementParentNode.replaceChild(oldStatementParentNode, newStatementNode);
        reformat(parentNode);

        return (JSStatement)newStatementNode.getPsi();
    }

    public static JSExpression replaceExpression(@Nonnull JSExpression expression, @NonNls @Nonnull String text)
        throws IncorrectOperationException {
        JSExpression newExpressionNode = JSChangeUtil.createExpressionFromText(
            expression.getProject(),
            text
        );

        return replaceExpression(expression, newExpressionNode);
    }

    public static JSExpression replaceExpression(@Nonnull JSExpression expression, @Nonnull JSExpression newExpression)
        throws IncorrectOperationException {
        ASTNode newExpressionNode = newExpression.getNode();
        ASTNode oldExpressionNode = expression.getNode();
        PsiElement parentNode = expression.getParent();
        ASTNode grandParentNode = parentNode.getNode();

        if (grandParentNode == null || oldExpressionNode == null || newExpressionNode == null) {
            return null;
        }

        grandParentNode.replaceChild(oldExpressionNode, newExpressionNode);
        reformat(parentNode);

        return (JSExpression)newExpressionNode.getPsi();
    }

    public static JSStatement replaceStatement(@Nonnull JSStatement statement, @NonNls @Nonnull String text)
        throws IncorrectOperationException {
        ASTNode newStatementNode = JSChangeUtil.createStatementFromText(
            statement.getProject(),
            text
        );

        ASTNode oldStatementParentNode = statement.getNode();
        PsiElement parentNode = statement.getParent();
        ASTNode newStatementParentNode = parentNode.getNode();

        if (newStatementParentNode == null || oldStatementParentNode == null || newStatementNode == null) {
            return null;
        }

        newStatementParentNode.replaceChild(oldStatementParentNode, newStatementNode);
        reformat(parentNode);

        return (JSStatement)newStatementNode.getPsi();
    }

    public static JSStatement addStatementBefore(@Nonnull JSStatement statement, @NonNls @Nonnull String previousStatementText)
        throws IncorrectOperationException {
        ASTNode newStatementNode = JSChangeUtil.createStatementFromText(
            statement.getProject(),
            previousStatementText
        );
        ASTNode oldStatementParentNode = statement.getNode();
        PsiElement parentNode = statement.getParent();
        ASTNode newStatementParentNode = parentNode.getNode();

        if (newStatementParentNode == null || newStatementNode == null) {
            return null;
        }

        newStatementParentNode.addChild(newStatementNode, oldStatementParentNode);
        reformat(parentNode);

        return (JSStatement)newStatementNode.getPsi();
    }

    public static JSStatement addStatementAfter(@Nonnull JSStatement statement, @NonNls @Nonnull String nextStatementText)
        throws IncorrectOperationException {
        ASTNode newStatementNode = JSChangeUtil.createStatementFromText(
            statement.getProject(),
            nextStatementText
        );
        ASTNode statementNode = statement.getNode();
        ASTNode oldStatementParentNode = ((statementNode == null) ? null : statementNode.getTreeNext());
        PsiElement parentNode = statement.getParent();
        ASTNode newStatementParentNode = parentNode.getNode();

        if (newStatementParentNode == null || newStatementNode == null) {
            return null;
        }

        newStatementParentNode.addChild(newStatementNode, oldStatementParentNode);
        reformat(parentNode);

        return (JSStatement)newStatementNode.getPsi();
    }

    public static void addRangeBefore(JSStatement[] statements, JSStatement statement) throws IncorrectOperationException {
        addRangeBefore(statements, 0, statements.length, statement);
    }

    public static void addRangeAfter(JSStatement[] statements, JSStatement statement) throws IncorrectOperationException {
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

    public static void replaceExpressionWithNegatedExpression(@Nonnull JSExpression newExpression, @Nonnull JSExpression exp)
        throws IncorrectOperationException {
        JSExpression expressionToReplace = BoolUtils.findNegation(exp);
        String replacementString;

        if (expressionToReplace == null) {
            expressionToReplace = exp;

            if (ComparisonUtils.isComparisonOperator(newExpression)) {
                JSBinaryExpression binaryExpression = (JSBinaryExpression)newExpression;
                IElementType operationSign = binaryExpression.getOperationSign();
                String negatedComparison = ComparisonUtils.getNegatedOperatorText(operationSign);
                JSExpression leftOperand = binaryExpression.getLOperand();
                JSExpression rightOperand = binaryExpression.getROperand();

                assert (rightOperand != null);

                replacementString = leftOperand.getText() + negatedComparison + rightOperand.getText();
            }
            else {
                replacementString = '!' + ParenthesesUtils.getParenthesized(newExpression, ParenthesesUtils.PREFIX_PRECENDENCE);
            }
        }
        else {
            replacementString = newExpression.getText();
        }
        replaceExpression(expressionToReplace, replacementString);
    }

    public static void replaceExpressionWithNegatedExpressionString(JSExpression exp, String newExpression)
        throws IncorrectOperationException {
        assert (exp != null);

        JSExpression expressionToReplace = BoolUtils.findNegation(exp);
        String replacementString = newExpression;

        if (expressionToReplace == null) {
            expressionToReplace = exp;
            replacementString = "!(" + newExpression + ')';
        }

        replaceExpression(expressionToReplace, replacementString);
    }

    public static void replaceStatementWithUnwrapping(JSStatement statement, JSStatement newBranch) throws IncorrectOperationException {
        if (!(newBranch instanceof JSBlockStatement)) {
            JSElementFactory.replaceStatement(statement, newBranch.getText());
            return;
        }

        JSBlockStatement parentBlock = PsiTreeUtil.getParentOfType(newBranch, JSBlockStatement.class);

        if (parentBlock == null) {
            JSElementFactory.replaceStatement(statement, newBranch.getText());
            return;
        }

        JSBlockStatement block = (JSBlockStatement)newBranch;

        if (ControlFlowUtils.containsConflictingDeclarations(block, parentBlock)) {
            JSElementFactory.replaceStatement(statement, newBranch.getText());
            return;
        }

        PsiElement containingElement = statement.getParent();

        assert (containingElement instanceof JSStatement);

        JSElementFactory.addRangeBefore(block.getStatements(), statement);
        JSElementFactory.removeElement(statement);
    }

    public static void removeElement(PsiElement element) {
        ASTNode node = element.getNode();
        ASTNode parentNode = element.getParent().getNode();

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
        ReadonlyStatusHandler instance = ReadonlyStatusHandler.getInstance(project);
        VirtualFile virtualFile = file.getVirtualFile();

        return virtualFile != null && instance.ensureFilesWritable(virtualFile).hasReadonlyFiles();
    }
}

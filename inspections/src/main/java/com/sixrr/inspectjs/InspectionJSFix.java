package com.sixrr.inspectjs;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSExpressionStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import consulo.annotation.access.RequiredWriteAction;
import consulo.language.ast.ASTNode;
import consulo.language.editor.inspection.LocalQuickFix;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.util.IncorrectOperationException;
import consulo.logging.Logger;
import consulo.project.Project;
import consulo.ui.annotation.RequiredUIAccess;
import consulo.virtualFileSystem.ReadonlyStatusHandler;
import consulo.virtualFileSystem.VirtualFile;

import jakarta.annotation.Nonnull;

public abstract class InspectionJSFix implements LocalQuickFix {
    @Override
    @RequiredUIAccess
    public void applyFix(@Nonnull Project project, @Nonnull ProblemDescriptor descriptor) {
        PsiElement problemElement = descriptor.getPsiElement();
        if (problemElement == null || !problemElement.isValid()) {
            return;
        }
        if (isQuickFixOnReadOnlyFile(problemElement)) {
            return;
        }
        try {
            doFix(project, descriptor);
        }
        catch (IncorrectOperationException e) {
            Class<? extends InspectionJSFix> aClass = getClass();
            String className = aClass.getName();
            Logger logger = Logger.getInstance(className);
            logger.error(e);
        }
    }

    protected abstract void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException;

    protected static void deleteElement(@Nonnull PsiElement element)
        throws IncorrectOperationException {
        ASTNode node = element.getNode();
        ASTNode parent = element.getParent().getNode();
        parent.removeChild(node);
    }

    @RequiredWriteAction
    protected static void replaceExpression(JSExpression expression, String newExpression) throws IncorrectOperationException {
        ASTNode fromText = JSChangeUtil.createStatementFromText(expression.getProject(), newExpression + ';');
        PsiElement element = fromText != null ? fromText.getPsi() : null;

        JSExpressionStatement expressionStatement = element instanceof JSExpressionStatement expr ? expr : null;
        if (expressionStatement == null) {
            return;
        }
        JSExpression newExp = expressionStatement.getExpression();
        ASTNode newExpressionNode = newExp.getNode();
        ASTNode expressionNode = expression.getNode();
        PsiElement parent = expression.getParent();
        ASTNode parentNode = parent.getNode();
        parentNode.replaceChild(expressionNode, newExpressionNode);
        // final CodeStyleManager styleManager = manager.getCodeStyleManager();
        // styleManager.reformat(newExpressionNode.getPsi());
    }

    @RequiredWriteAction
    protected static void replaceStatement(JSStatement statement, String newStatement) throws IncorrectOperationException {
        ASTNode fromText = JSChangeUtil.createStatementFromText(statement.getProject(), newStatement);
        PsiElement element = fromText != null ? fromText.getPsi() : null;
        JSStatement newStmt = element instanceof JSStatement newStmt1 ? newStmt1 : null;

        if (newStmt == null) {
            return;
        }
        PsiElement parent = statement.getParent();
        ASTNode parentNode = parent.getNode();
        ASTNode statementNode = statement.getNode();
        ASTNode newStatementNode = newStmt.getNode();
        parentNode.replaceChild(statementNode, newStatementNode);
        // final CodeStyleManager styleManager = manager.getCodeStyleManager();
        // styleManager.reformat(newStatementNode.getPsi());
    }

    private static boolean isQuickFixOnReadOnlyFile(PsiElement problemElement) {
        PsiFile containingPsiFile = problemElement.getContainingFile();
        if (containingPsiFile == null) {
            return false;
        }
        VirtualFile virtualFile = containingPsiFile.getVirtualFile();
        Project project = problemElement.getProject();
        ReadonlyStatusHandler handler = ReadonlyStatusHandler.getInstance(project);
        ReadonlyStatusHandler.OperationStatus status = handler.ensureFilesWritable(virtualFile);
        return status.hasReadonlyFiles();
    }
}

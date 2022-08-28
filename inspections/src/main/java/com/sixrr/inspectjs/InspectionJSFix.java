package com.sixrr.inspectjs;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSExpressionStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import consulo.language.ast.ASTNode;
import consulo.language.editor.inspection.LocalQuickFix;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.util.IncorrectOperationException;
import consulo.logging.Logger;
import consulo.project.Project;
import consulo.virtualFileSystem.ReadonlyStatusHandler;
import consulo.virtualFileSystem.VirtualFile;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;

public abstract class InspectionJSFix implements LocalQuickFix
{
    //to appear in "Apply Fix" statement when multiple Quick Fixes exist
    @Override
	@Nonnull
    public String getFamilyName() {
        return "";
    }

    @Override
	public void applyFix(@Nonnull Project project,
                         @Nonnull ProblemDescriptor descriptor) {
        final PsiElement problemElement = descriptor.getPsiElement();
        if (problemElement == null || !problemElement.isValid()) {
            return;
        }
        if (isQuickFixOnReadOnlyFile(problemElement)) {
            return;
        }
        try {
            doFix(project, descriptor);
        } catch (IncorrectOperationException e) {
            final Class<? extends InspectionJSFix> aClass = getClass();
            final String className = aClass.getName();
            final Logger logger = Logger.getInstance(className);
            logger.error(e);
        }
    }

    protected abstract void doFix(Project project, ProblemDescriptor descriptor)
            throws IncorrectOperationException;

    protected static void deleteElement(@Nonnull PsiElement element)
            throws IncorrectOperationException {
        final ASTNode node = element.getNode();
        final ASTNode parent = element.getParent().getNode();
        parent.removeChild(node);
    }

    protected static void replaceExpression(JSExpression expression,
                                            String newExpression)
            throws IncorrectOperationException {

        final ASTNode fromText = JSChangeUtil.createStatementFromText(expression.getProject(), newExpression + ';');
        final PsiElement element = fromText != null ? fromText.getPsi() : null;

        final JSExpressionStatement expressionStatement = element instanceof JSExpressionStatement ? (JSExpressionStatement) element:null;
        if (expressionStatement == null) {
            return;
        }
        final JSExpression newExp = expressionStatement.getExpression();
        final ASTNode newExpressionNode = newExp.getNode();
        final ASTNode expressionNode = expression.getNode();
        final PsiElement parent = expression.getParent();
        final ASTNode parentNode = parent.getNode();
        parentNode.replaceChild(expressionNode, newExpressionNode);
       // final CodeStyleManager styleManager = manager.getCodeStyleManager();
       // styleManager.reformat(newExpressionNode.getPsi());
    }

    protected static void replaceStatement(JSStatement statement,
                                           @NonNls String newStatement)
            throws IncorrectOperationException {
        final ASTNode fromText = JSChangeUtil.createStatementFromText(statement.getProject(), newStatement);
        final PsiElement element = fromText != null ? fromText.getPsi() : null;
        final JSStatement newStmt = element instanceof JSStatement ? (JSStatement)element:null;
      
        if (newStmt == null) {
            return;
        }
        final PsiElement parent = statement.getParent();
        final ASTNode parentNode = parent.getNode();
        final ASTNode statementNode = statement.getNode();
        final ASTNode newStatementNode = newStmt.getNode();
        parentNode.replaceChild(statementNode, newStatementNode);
       // final CodeStyleManager styleManager = manager.getCodeStyleManager();
       // styleManager.reformat(newStatementNode.getPsi());
    }

    private static boolean isQuickFixOnReadOnlyFile(PsiElement problemElement) {
        final PsiFile containingPsiFile = problemElement.getContainingFile();
        if (containingPsiFile == null) {
          return false;
        }
        final VirtualFile virtualFile = containingPsiFile.getVirtualFile();
        final Project project = problemElement.getProject();
        final ReadonlyStatusHandler handler = ReadonlyStatusHandler.getInstance(project);
        final ReadonlyStatusHandler.OperationStatus status = handler.ensureFilesWritable(virtualFile);
        return status.hasReadonlyFiles();
    }
}

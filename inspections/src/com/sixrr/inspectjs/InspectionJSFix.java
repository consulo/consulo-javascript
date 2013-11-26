package com.sixrr.inspectjs;

import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.lang.ASTNode;
import com.intellij.lang.Language;
import com.intellij.lang.javascript.JSLanguageDialect;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.ReadonlyStatusHandler;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public abstract class InspectionJSFix implements LocalQuickFix {
    //to appear in "Apply Fix" statement when multiple Quick Fixes exist
    @NotNull
    public String getFamilyName() {
        return "";
    }

    public void applyFix(@NotNull Project project,
                         @NotNull ProblemDescriptor descriptor) {
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

    protected static void deleteElement(@NotNull PsiElement element)
            throws IncorrectOperationException {
        final ASTNode node = element.getNode();
        final ASTNode parent = element.getParent().getNode();
        parent.removeChild(node);
    }

    protected static void replaceExpression(JSExpression expression,
                                            String newExpression)
            throws IncorrectOperationException {

        final JSLanguageDialect javascript = getLanguageDialect(expression);
        final ASTNode fromText = JSChangeUtil.createStatementFromText(expression.getProject(), newExpression + ';', javascript);
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
        final JSLanguageDialect javascript = getLanguageDialect(statement);
        final ASTNode fromText = JSChangeUtil.createStatementFromText(statement.getProject(), newStatement, javascript);
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

    private static @Nullable JSLanguageDialect getLanguageDialect(final JSElement statement) {
        final PsiFile containingFile = statement.getContainingFile();
        if (!(containingFile instanceof JSFile)) return null;
        final Language language = containingFile.getLanguage();
        if (!(language instanceof JSLanguageDialect)) return null;
        return (JSLanguageDialect)language;
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

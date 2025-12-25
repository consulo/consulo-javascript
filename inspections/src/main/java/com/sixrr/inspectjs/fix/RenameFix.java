package com.sixrr.inspectjs.fix;

import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.dataContext.DataManager;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.editor.refactoring.RefactoringFactory;
import consulo.language.editor.refactoring.RenameRefactoring;
import consulo.language.editor.refactoring.action.RefactoringActionHandler;
import consulo.language.editor.refactoring.action.RefactoringActionHandlerFactory;
import consulo.language.psi.PsiElement;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import jakarta.annotation.Nonnull;

public class RenameFix extends InspectionJSFix {
    private final String m_targetName;

    public RenameFix() {
        super();
        m_targetName = null;
    }

    public RenameFix(String targetName) {
        super();
        m_targetName = targetName;
    }

    @Override
    @Nonnull
    public LocalizeValue getName() {
        return m_targetName == null
            ? InspectionJSLocalize.renameFix()
            : InspectionJSLocalize.renameToFix(m_targetName);
    }

    @Override
    public void doFix(Project project, ProblemDescriptor descriptor) {
        PsiElement nameIdentifier = descriptor.getPsiElement();
        PsiElement elementToRename = nameIdentifier.getParent();
        if (m_targetName == null) {
            RefactoringActionHandlerFactory factory = RefactoringActionHandlerFactory.getInstance();
            RefactoringActionHandler renameHandler = factory.createRenameHandler();
            renameHandler.invoke(project, new PsiElement[]{elementToRename}, DataManager.getInstance().getDataContext());
        }
        else {
            RefactoringFactory factory = RefactoringFactory.getInstance(project);
            RenameRefactoring renameRefactoring = factory.createRename(elementToRename, m_targetName);
            renameRefactoring.run();
        }
    }
}

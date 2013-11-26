package com.sixrr.inspectjs.fix;

import com.intellij.openapi.project.Project;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.psi.PsiElement;
import com.intellij.refactoring.RefactoringActionHandlerFactory;
import com.intellij.refactoring.RefactoringActionHandler;
import com.intellij.refactoring.RefactoringFactory;
import com.intellij.refactoring.RenameRefactoring;
import com.intellij.ide.DataManager;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.InspectionJSBundle;
import org.jetbrains.annotations.NotNull;

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

    @NotNull
    public String getName() {
        if (m_targetName == null) {
            return InspectionJSBundle.message("rename.fix");
        } else {
            return InspectionJSBundle.message("rename.to.fix", m_targetName);
        }
    }

    public void doFix(Project project, ProblemDescriptor descriptor) {
        final PsiElement nameIdentifier = descriptor.getPsiElement();
        final PsiElement elementToRename = nameIdentifier.getParent();
        if (m_targetName == null) {
            final RefactoringActionHandlerFactory factory =
                    RefactoringActionHandlerFactory.getInstance();
            final RefactoringActionHandler renameHandler = factory.createRenameHandler();
            renameHandler.invoke(project, new PsiElement[]{elementToRename}, DataManager.getInstance().getDataContext());
        } else {
            final RefactoringFactory factory = RefactoringFactory.getInstance(project);
            final RenameRefactoring renameRefactoring = factory.createRename(elementToRename, m_targetName);
            renameRefactoring.run();
        }
    }
}

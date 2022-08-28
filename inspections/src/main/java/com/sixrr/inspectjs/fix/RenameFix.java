package com.sixrr.inspectjs.fix;

import consulo.language.editor.refactoring.RefactoringFactory;
import consulo.project.Project;
import consulo.dataContext.DataManager;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.editor.refactoring.action.RefactoringActionHandlerFactory;
import consulo.language.editor.refactoring.action.RefactoringActionHandler;
import consulo.language.editor.refactoring.RenameRefactoring;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.InspectionJSBundle;
import javax.annotation.Nonnull;

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
    public String getName() {
        if (m_targetName == null) {
            return InspectionJSBundle.message("rename.fix");
        } else {
            return InspectionJSBundle.message("rename.to.fix", m_targetName);
        }
    }

    @Override
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

package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.ControlFlowUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import jakarta.annotation.Nonnull;
import org.intellij.lang.annotations.Pattern;

@ExtensionImpl
public class  UnnecessaryReturnJSInspection extends JavaScriptInspection {
    private final UnnecessaryReturnFix fix = new UnnecessaryReturnFix();

    @Nonnull
    @Override
    @Pattern(value = "[a-zA-Z_0-9.-]+")
    public String getID() {
        return "UnnecessaryReturnStatementJS";
    }

    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.unnecessaryReturnStatementDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @Override
    public boolean isEnabledByDefault() {
        return true;
    }

    @RequiredReadAction
    @Override
    public String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.unnecessaryReturnErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new UnnecessaryReturnVisitor();
    }

    @Override
    public InspectionJSFix buildFix(PsiElement location, Object state) {
        return fix;
    }

    private static class UnnecessaryReturnFix extends InspectionJSFix {
        @Nonnull
        @Override
        public LocalizeValue getName() {
            return InspectionJSLocalize.removeUnnecessaryReturnFix();
        }

        @Override
        public void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException {
            final PsiElement returnKeywordElement = descriptor.getPsiElement();
            final PsiElement returnStatement = returnKeywordElement.getParent();
            assert returnStatement != null;
            deleteElement(returnStatement);
        }
    }

    private static class UnnecessaryReturnVisitor extends BaseInspectionVisitor {
        @Override
        public void visitJSReturnStatement(@Nonnull JSReturnStatement statement) {
            super.visitJSReturnStatement(statement);

            final JSExpression returnValue = statement.getExpression();
            if(returnValue!=null)
            {
                return;
            }
            final JSFunction function = PsiTreeUtil.getParentOfType(statement, JSFunction.class);
            if (function == null) {
                return;
            }
            final PsiElement body = function.getLastChild();
            if (body == null) {
                return;
            }
            if(!(body instanceof JSBlockStatement))
            {
                return;
            }
            if (ControlFlowUtils.statementCompletesWithStatement((JSStatement) body, statement)) {
                registerStatementError(statement);
            }
        }
    }
}

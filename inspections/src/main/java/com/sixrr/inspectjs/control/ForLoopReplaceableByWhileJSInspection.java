package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSForStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSVarStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import jakarta.annotation.Nonnull;
import org.intellij.lang.annotations.Pattern;
import org.jetbrains.annotations.NonNls;

@ExtensionImpl
public class ForLoopReplaceableByWhileJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    @Pattern(value = "[a-zA-Z_0-9.-]+")
    public String getID() {
        return "ForLoopReplaceableByWhile";
    }

    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.forLoopReplaceableByWhileDisplayName();
    }

    @Override
    @Nonnull
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @Nonnull
    @Override
    @RequiredReadAction
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.forLoopReplaceableByWhileProblemDescriptor().get();
    }

    @Nonnull
    @Override
    public InspectionToolState<?> createStateProvider() {
        return new ForLoopReplaceableByWhileJSInspectionState();
    }

    @Override
    public InspectionJSFix buildFix(PsiElement location, Object state) {
        return new ReplaceForByWhileFix();
    }

    private static class ReplaceForByWhileFix extends InspectionJSFix {
        @Nonnull
        @Override
        public LocalizeValue getName() {
            return InspectionJSLocalize.forLoopReplaceableByWhileReplaceQuickfix();
        }

        @Override
        public void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException {
            PsiElement forKeywordElement = descriptor.getPsiElement();
            JSForStatement forStatement = (JSForStatement) forKeywordElement.getParent();
            assert forStatement != null;
            JSExpression condition = forStatement.getCondition();
            JSStatement body = forStatement.getBody();
            String bodyText = body == null ? "" : body.getText();
            @NonNls String whileStatement;
            if (condition == null) {
                whileStatement = "while(true)" + bodyText;
            }
            else {
                whileStatement = "while(" + condition.getText() + ')' + bodyText;
            }
            replaceStatement(forStatement, whileStatement);
        }
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new ForLoopReplaceableByWhileVisitor();
    }

    private class ForLoopReplaceableByWhileVisitor extends BaseInspectionVisitor<ForLoopReplaceableByWhileJSInspectionState> {
        @Override
        public void visitJSForStatement(@Nonnull JSForStatement statement) {
            super.visitJSForStatement(statement);
            JSVarStatement varStatement = statement.getVarDeclaration();
            if (varStatement != null) {
                return;
            }
            JSExpression initialization = statement.getInitialization();
            if (initialization != null) {
                return;
            }
            JSExpression update = statement.getUpdate();
            if (update != null) {
                return;
            }
            if (myState.m_ignoreLoopsWithoutConditions) {
                JSExpression condition = statement.getCondition();
                if (condition == null) {
                    return;
                }
                String conditionText = condition.getText();
                if ("true".equals(conditionText)) {
                    return;
                }
            }
            registerStatementError(statement);
        }
    }
}
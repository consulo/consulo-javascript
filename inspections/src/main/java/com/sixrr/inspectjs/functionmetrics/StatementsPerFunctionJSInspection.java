package com.sixrr.inspectjs.functionmetrics;

import com.intellij.lang.javascript.psi.JSBlockStatement;
import com.intellij.lang.javascript.psi.JSFunction;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.language.psi.PsiElement;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class StatementsPerFunctionJSInspection extends JavaScriptInspection {
    @Override
    @Nonnull
    public String getID() {
        return "FunctionTooLongJS";
    }

    @Override
    @Nonnull
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.overlyLongFunctionDisplayName();
    }

    @Override
    @Nonnull
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.FUNCTIONMETRICS_GROUP_NAME;
    }

    @Nonnull
    @Override
    public InspectionToolState<?> createStateProvider() {
        return new StatementsPerFunctionJSInspectionState();
    }

    @RequiredReadAction
    @Override
    public String buildErrorString(Object state, Object... args) {
        JSFunction function = (JSFunction)((PsiElement)args[0]).getParent();
        assert function != null;
        PsiElement lastChild = function.getLastChild();
        StatementCountVisitor visitor = new StatementCountVisitor();
        assert lastChild != null;
        lastChild.accept(visitor);
        int coupling = visitor.getStatementCount();
        return functionHasIdentifier(function)
            ? InspectionJSLocalize.functionIsOverlyLongStatementErrorString(coupling).get()
            : InspectionJSLocalize.anonymousFunctionIsOverlyLongStatementErrorString(coupling).get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private class Visitor extends BaseInspectionVisitor<StatementsPerFunctionJSInspectionState> {
        @Override
        public void visitJSFunctionDeclaration(@Nonnull JSFunction function) {

            PsiElement lastChild = function.getLastChild();
            if (!(lastChild instanceof JSBlockStatement)) {
                return;
            }
            StatementCountVisitor visitor = new StatementCountVisitor();
            lastChild.accept(visitor);
            int statementCount = visitor.getStatementCount();

            if (statementCount <= myState.getLimit()) {
                return;
            }
            registerFunctionError(function);
        }
    }
}

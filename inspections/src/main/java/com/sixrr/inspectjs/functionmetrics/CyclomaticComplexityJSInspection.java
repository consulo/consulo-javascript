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
import org.intellij.lang.annotations.Pattern;

@ExtensionImpl
public class CyclomaticComplexityJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    @Pattern(value = "[a-zA-Z_0-9.-]+")
    public String getID() {
        return "OverlyComplexFunctionJS";
    }

    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.overlyComplexFunctionDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.FUNCTIONMETRICS_GROUP_NAME;
    }

    @Nonnull
    @Override
    public InspectionToolState<?> createStateProvider() {
        return new CyclomaticComplexityJSInspectionState();
    }

    @Override
    @RequiredReadAction
    public String buildErrorString(Object state, Object... args) {
        JSFunction function = (JSFunction)((PsiElement)args[0]).getParent();
        assert function != null;
        CyclomaticComplexityVisitor visitor = new CyclomaticComplexityVisitor();
        PsiElement lastChild = function.getLastChild();
        assert lastChild != null;
        lastChild.accept(visitor);
        int coupling = visitor.getComplexity();
        return functionHasIdentifier(function)
            ? InspectionJSLocalize.functionRefIsOverlyComplexCyclomaticComplexityErrorString(coupling).get()
            : InspectionJSLocalize.anonymousFunctionIsOverlyComplexCyclomaticComplexityErrorString(coupling).get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private class Visitor extends BaseInspectionVisitor<CyclomaticComplexityJSInspectionState> {
        @Override
        public void visitJSFunctionDeclaration(@Nonnull JSFunction function) {
            PsiElement lastChild = function.getLastChild();
            if (!(lastChild instanceof JSBlockStatement)) {
                return;
            }
            CyclomaticComplexityVisitor visitor = new CyclomaticComplexityVisitor();
            lastChild.accept(visitor);
            int complexity = visitor.getComplexity();

            if (complexity <= myState.getLimit()) {
                return;
            }
            registerFunctionError(function);
        }
    }
}

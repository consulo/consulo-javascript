package com.sixrr.inspectjs.functionmetrics;

import com.intellij.lang.javascript.psi.JSFunction;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.language.psi.PsiElement;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class NestingDepthJSInspection extends JavaScriptInspection {
    @Override
    @Nonnull
    public String getID() {
        return "OverlyNestedFunctionJS";
    }

    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.overlyNestedFunctionDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.FUNCTIONMETRICS_GROUP_NAME.get();
    }

    @Nonnull
    @Override
    public InspectionToolState<?> createStateProvider() {
        return new NestingDepthJSInspectionState();
    }

    @RequiredReadAction
    @Override
    public String buildErrorString(Object state, Object... args) {
        final JSFunction function = (JSFunction)((PsiElement)args[0]).getParent();
        assert function != null;
        final NestingDepthVisitor visitor = new NestingDepthVisitor();
        function.accept(visitor);
        final int nestingDepth = visitor.getMaximumDepth();
        return functionHasIdentifier(function)
            ? InspectionJSLocalize.functionIsOverlyNestedErrorString(nestingDepth).get()
            : InspectionJSLocalize.anonymousFunctionIsOverlyNestedErrorString(nestingDepth).get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private class Visitor extends BaseInspectionVisitor<NestingDepthJSInspectionState> {
        @Override
        public void visitJSFunctionDeclaration(@Nonnull JSFunction function) {
            final NestingDepthVisitor visitor = new NestingDepthVisitor();
            function.accept(visitor);
            final int count = visitor.getMaximumDepth();

            if (count > myState.getLimit()) {
                registerFunctionError(function);
            }
        }
    }
}

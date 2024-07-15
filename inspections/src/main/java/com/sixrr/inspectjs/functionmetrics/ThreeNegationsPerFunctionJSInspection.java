package com.sixrr.inspectjs.functionmetrics;

import com.intellij.lang.javascript.psi.JSBlockStatement;
import com.intellij.lang.javascript.psi.JSFunction;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.PsiElement;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class ThreeNegationsPerFunctionJSInspection extends JavaScriptInspection {
    @Override
    @Nonnull
    public String getID() {
        return "FunctionWithMoreThanThreeNegationsJS";
    }

    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.functionWithMoreThanThreeNegationsDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.FUNCTIONMETRICS_GROUP_NAME.get();
    }

    @RequiredReadAction
    @Override
    public String buildErrorString(Object state, Object... args) {
        final JSFunction function = (JSFunction) ((PsiElement) args[0]).getParent();
        assert function != null;
        final PsiElement lastChild = function.getLastChild();
        final NegationCountVisitor visitor = new NegationCountVisitor();
        assert lastChild != null;
        lastChild.accept(visitor);
        final int negationCount = visitor.getNegationCount();
        return functionHasIdentifier(function)
            ? InspectionJSLocalize.functionContainsTooManyNegationErrorString(negationCount).get()
            : InspectionJSLocalize.anonymousFunctionContainsTooManyNegationErrorString(negationCount).get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSFunctionDeclaration(@Nonnull JSFunction function) {
            final NegationCountVisitor visitor = new NegationCountVisitor();
            final PsiElement lastChild = function.getLastChild();
            if (!(lastChild instanceof JSBlockStatement)) {
                return;
            }
            lastChild.accept(visitor);
            final int negationCount = visitor.getNegationCount();
            if (negationCount <= 3) {
                return;
            }
            registerFunctionError(function);
        }
    }
}

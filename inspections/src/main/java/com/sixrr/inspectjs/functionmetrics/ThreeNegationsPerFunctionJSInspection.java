package com.sixrr.inspectjs.functionmetrics;

import javax.annotation.Nonnull;

import com.intellij.lang.javascript.psi.JSBlockStatement;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.psi.PsiElement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;

public class ThreeNegationsPerFunctionJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getID() {
        return "FunctionWithMoreThanThreeNegationsJS";
    }

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("function.with.more.than.three.negations.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.FUNCTIONMETRICS_GROUP_NAME;
    }

    @Override
	public String buildErrorString(Object... args) {
        final JSFunction function = (JSFunction) ((PsiElement) args[0]).getParent();
        assert function != null;
        final PsiElement lastChild = function.getLastChild();
        final NegationCountVisitor visitor = new NegationCountVisitor();
        assert lastChild != null;
        lastChild.accept(visitor);
        final int negationCount = visitor.getNegationCount();
        if (functionHasIdentifier(function)) {
            return InspectionJSBundle.message("function.contains.too.many.negation.error.string", negationCount);
        } else {
            return InspectionJSBundle.message("anonymous.function.contains.too.many.negation.error.string", negationCount);
        }
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {

        @Override public void visitJSFunctionDeclaration(@Nonnull JSFunction function) {
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

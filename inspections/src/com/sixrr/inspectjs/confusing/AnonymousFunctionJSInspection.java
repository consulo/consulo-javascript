package com.sixrr.inspectjs.confusing;

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSFunctionExpression;
import com.intellij.psi.util.PsiTreeUtil;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class AnonymousFunctionJSInspection extends JavaScriptInspection{

    @Override
	@NotNull
    public String getDisplayName(){
        return InspectionJSBundle.message("anonymous.function.display.name");
    }

    @Override
	@NotNull
    public String getGroupDisplayName(){
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    @Override
	@Nullable
    protected String buildErrorString(Object... args){
        return InspectionJSBundle.message("anonymous.function.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor(){
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor{

        @Override public void visitJSFunctionExpression(JSFunctionExpression jsFunctionExpression){
            super.visitJSFunctionExpression(jsFunctionExpression);
            final JSFunction function = jsFunctionExpression.getFunction();
            final ASTNode identifier = function.findNameIdentifier();
            if(identifier == null ||
                    PsiTreeUtil.isAncestor(function, identifier.getPsi(), true)){
                return;
            }
            registerError(function.getFirstChild());
        }
    }
}

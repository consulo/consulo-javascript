package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSFunctionExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.PsiElement;
import consulo.language.psi.util.PsiTreeUtil;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@ExtensionImpl
public class AnonymousFunctionJSInspection extends JavaScriptInspection{

    @Override
	@Nonnull
    public String getDisplayName(){
        return InspectionJSBundle.message("anonymous.function.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName(){
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    @RequiredReadAction
	@Override
	@Nullable
    protected String buildErrorString(Object state, Object... args){
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
            final PsiElement identifier = function.getNameIdentifier();
            if(identifier == null ||
                    PsiTreeUtil.isAncestor(function, identifier, true)){
                return;
            }
            registerError(function.getFirstChild());
        }
    }
}

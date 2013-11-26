package com.intellij.javascript;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.template.Expression;
import com.intellij.codeInsight.template.ExpressionContext;
import com.intellij.codeInsight.template.Macro;
import com.intellij.codeInsight.template.Result;
import com.intellij.codeInsight.template.TextResult;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSFunctionExpression;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;

public class JSMethodNameMacro extends Macro {

  @NonNls
  public String getName() {
    return "jsMethodName";
  }

	@Override
	public String getPresentableName()
	{
		return JSBundle.message("js.methodname.macro.description");
	}


  @NonNls
  public String getDefaultValue() {
    return "";
  }

  public Result calculateResult(@NotNull Expression[] params, ExpressionContext context) {
    final PsiElement elementAtCaret = JSClassNameMacro.findElementAtCaret(context);
    if (elementAtCaret != null) {
      JSFunction function = PsiTreeUtil.getParentOfType(elementAtCaret, JSFunction.class);
      if (function instanceof JSFunctionExpression) {
        function = ((JSFunctionExpression)function).getFunction();
      }

      if (function != null) {
        final String name = function.getName();
        if (name != null) return new TextResult(name);
      }
    }
    return null;
  }

  public Result calculateQuickResult(@NotNull Expression[] params, ExpressionContext context) {
    return null;
  }

  public LookupElement[] calculateLookupItems(@NotNull Expression[] params, ExpressionContext context) {
    return null;
  }
}

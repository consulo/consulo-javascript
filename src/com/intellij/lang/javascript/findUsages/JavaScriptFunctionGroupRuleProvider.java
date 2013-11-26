package com.intellij.lang.javascript.findUsages;

import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSNamedElement;
import com.intellij.usages.UsageGroup;
import com.intellij.util.Icons;
import org.jetbrains.annotations.NotNull;

/**
 * @author Maxim.Mossienko
 */
public class JavaScriptFunctionGroupRuleProvider extends JavaScriptGroupRuleProviderBase<JSFunction> {
  protected Class<? extends JSNamedElement> getUsageClass() {
    return JSFunction.class;
  }

  protected UsageGroup createUsageGroup(final JSFunction jsFunction) {
    return new FunctionUsageGroup(jsFunction);
  }

  private static class FunctionUsageGroup extends JavaScriptGroupRuleProviderBase.PsiNamedElementUsageGroupBase<JSFunction> {
    public FunctionUsageGroup(@NotNull JSFunction function) {
      super(function, Icons.METHOD_ICON);
    }
  }
}

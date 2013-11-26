package com.intellij.lang.javascript.findUsages;

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSNamedElement;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.usages.UsageGroup;
import com.intellij.util.Icons;
import org.jetbrains.annotations.NotNull;

/**
 * @author Maxim.Mossienko
 */
public class JavaScriptClassGroupRuleProvider extends JavaScriptGroupRuleProviderBase<JSClass> {
  protected Class<? extends JSNamedElement> getUsageClass() {
    return JSClass.class;
  }

  protected UsageGroup createUsageGroup(final JSClass clazz) {
    return new ClassUsageGroup(clazz);
  }

  @Override
  protected boolean isAcceptableElement(JSNamedElement element) {
    return super.isAcceptableElement(element) && !JSResolveUtil.isArtificialClassUsedForReferenceList((JSClass)element);
  }

  private static class ClassUsageGroup extends JavaScriptGroupRuleProviderBase.PsiNamedElementUsageGroupBase<JSClass> {
    public ClassUsageGroup(@NotNull JSClass clazz) {
      super(clazz, Icons.CLASS_ICON);
    }
  }
}
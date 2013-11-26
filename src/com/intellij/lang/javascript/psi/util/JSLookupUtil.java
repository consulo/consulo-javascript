package com.intellij.lang.javascript.psi.util;

import com.intellij.openapi.components.ApplicationComponent;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.lang.javascript.psi.JSNamedElement;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.Nullable;

/**
 * Created by IntelliJ IDEA.
 * User: maxim.mossienko
 * Date: Dec 6, 2005
 * Time: 8:35:58 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class JSLookupUtil implements ApplicationComponent {
  @NonNls
  public String getComponentName() {
    return "JS.LookupUtil";
  }

  public void initComponent() {}

  public void disposeComponent() {
  }

  @Nullable
  public abstract Object createPrioritizedLookupItem(PsiElement value, String name, int priority);

  public static JSLookupUtil getInstance() {
    return ApplicationManager.getApplication().getComponent(JSLookupUtil.class);
  }
}

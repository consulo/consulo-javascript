package com.intellij.lang.javascript.index;

import com.intellij.navigation.ChooseByNameContributor;
import com.intellij.navigation.NavigationItem;
import com.intellij.openapi.project.Project;
import com.intellij.util.ArrayUtil;

/**
 * Created by IntelliJ IDEA.
 * User: yole
 * Date: 03.10.2005
 * Time: 14:45:22
 * To change this template use File | Settings | File Templates.
 */
public class JavaScriptSymbolContributor implements ChooseByNameContributor {
  public String[] getNames(Project project, boolean includeNonProjectItems) {
    JavaScriptIndex index = JavaScriptIndex.getInstance(project);
    return index != null ? index.getSymbolNames(includeNonProjectItems): ArrayUtil.EMPTY_STRING_ARRAY;
  }

  public NavigationItem[] getItemsByName(String name, final String pattern, Project project, boolean includeNonProjectItems) {
    JavaScriptIndex index = JavaScriptIndex.getInstance(project);
    return index != null ? index.getSymbolsByName(name, includeNonProjectItems):NavigationItem.EMPTY_NAVIGATION_ITEM_ARRAY;
  }
}

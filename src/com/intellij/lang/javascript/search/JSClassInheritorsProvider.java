package com.intellij.lang.javascript.search;

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.openapi.extensions.ExtensionPointName;
import com.intellij.openapi.project.Project;
import com.intellij.psi.search.GlobalSearchScope;

import java.util.Collection;

public interface JSClassInheritorsProvider {

  ExtensionPointName<JSClassInheritorsProvider> EP_NAME = ExtensionPointName.create("JavaScript.classInheritorsProvider");

  Collection<JSClass> getImplementingClasses(String parentName, Project project, GlobalSearchScope scope);

  Collection<JSClass> getExtendingClasses(String parentName, Project project, GlobalSearchScope scope);

}

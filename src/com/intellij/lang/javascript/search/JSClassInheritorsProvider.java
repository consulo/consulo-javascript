package com.intellij.lang.javascript.search;

import java.util.Collection;

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.openapi.extensions.ExtensionPointName;
import com.intellij.openapi.project.Project;
import com.intellij.psi.search.GlobalSearchScope;

public interface JSClassInheritorsProvider
{

	ExtensionPointName<JSClassInheritorsProvider> EP_NAME = ExtensionPointName.create("org.mustbe.consulo.javascript.classInheritorsProvider");

	Collection<JSClass> getImplementingClasses(String parentName, Project project, GlobalSearchScope scope);

	Collection<JSClass> getExtendingClasses(String parentName, Project project, GlobalSearchScope scope);
}

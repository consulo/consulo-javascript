package com.intellij.lang.javascript.flex;

import org.jetbrains.annotations.Nullable;
import com.intellij.lang.javascript.index.JavaScriptIndex;
import com.intellij.openapi.extensions.ExtensionPointName;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.Processor;

/**
 * @author yole
 */
public interface JSResolveHelper
{
	ExtensionPointName<JSResolveHelper> EP_NAME = ExtensionPointName.create("org.mustbe.consulo.javascript.resolveHelper");

	// TODO: drop module
	@Nullable
	PsiElement findClassByQName(final String link, final JavaScriptIndex index, final String className, GlobalSearchScope scope);

	void importClass(final PsiScopeProcessor processor, final PsiNamedElement file, final String packageQualifierText);

	boolean processPackage(final String packageQualifierText, String resolvedName, final Processor<VirtualFile> processor,
			GlobalSearchScope globalSearchScope, Project project);
}

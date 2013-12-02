package com.intellij.lang.javascript.psi;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.impl.JSExpressionCodeFragmentImpl;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;

/**
 * @author nik
 */
public class JSElementFactory
{
	private JSElementFactory()
	{
	}

	@NotNull
	public static JSFile createExpressionCodeFragment(@NotNull Project project, CharSequence text, PsiElement context, boolean isPhysical)
	{
		@NonNls String name = "fragment." + JavaScriptSupportLoader.JAVASCRIPT.getDefaultExtension();
		JSExpressionCodeFragmentImpl codeFragment = new JSExpressionCodeFragmentImpl(project, name, text, isPhysical);
		codeFragment.setContext(context);
		return codeFragment;
	}

}

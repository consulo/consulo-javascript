package com.intellij.lang.javascript.psi.resolve;

import com.intellij.lang.javascript.psi.JSImportStatement;
import com.intellij.psi.PsiElement;

/**
 * @author Maxim.Mossienko
 *         Date: Jul 29, 2008
 *         Time: 8:04:55 PM
 */
public class JSImportedElementResolveResult
{
	public final String qualifiedName;
	public final PsiElement resolvedElement;
	public final JSImportStatement importStatement;
	public static final JSImportedElementResolveResult EMPTY_RESULT = new JSImportedElementResolveResult(null);

	public JSImportedElementResolveResult(String _qualifiedName)
	{
		this(_qualifiedName, null, null);
	}

	public JSImportedElementResolveResult(String _qualifiedName, PsiElement _resolvedElement, JSImportStatement _importString)
	{
		qualifiedName = _qualifiedName;
		resolvedElement = _resolvedElement;
		importStatement = _importString;
	}

	public JSImportedElementResolveResult appendSignature(final String s)
	{
		return new JSImportedElementResolveResult(qualifiedName + s, resolvedElement, importStatement);
	}
}

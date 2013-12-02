package com.intellij.lang.javascript.psi;

import org.jetbrains.annotations.Nullable;
import com.intellij.lang.ASTNode;
import com.intellij.navigation.NavigationItem;
import com.intellij.psi.PsiNameIdentifierOwner;
import com.intellij.psi.PsiNamedElement;

/**
 * @author ven
 */
public interface JSNamedElement extends PsiNamedElement, JSElement, NavigationItem, PsiNameIdentifierOwner
{
	@Nullable
	ASTNode findNameIdentifier();
}

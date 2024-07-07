package com.intellij.lang.javascript.psi.impl;

import consulo.language.ast.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSFunctionProperty;
import com.intellij.lang.javascript.psi.JSParameterList;
import com.intellij.lang.javascript.psi.JSSourceElement;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.psi.JavaScriptType;
import consulo.javascript.language.psi.JavaScriptTypeElement;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * @author VISTALL
 * @since 19/12/2021
 */
public class JSFunctionPropertyImpl extends JSPropertyImpl implements JSFunctionProperty
{
	public JSFunctionPropertyImpl(ASTNode node)
	{
		super(node);
	}

	@RequiredReadAction
	@Override
	public boolean isGetProperty()
	{
		return findChildByType(JSTokenTypes.GET_KEYWORD) != null;
	}

	@RequiredReadAction
	@Override
	public boolean isSetProperty()
	{
		return findChildByType(JSTokenTypes.SET_KEYWORD) != null;
	}

	@RequiredReadAction
	@Nullable
	@Override
	public JSParameterList getParameterList()
	{
		return (JSParameterList) findChildByType(JSElementTypes.PARAMETER_LIST);
	}

	@Override
	public JSSourceElement[] getBody()
	{
		final ASTNode[] children = getNode().getChildren(JSElementTypes.SOURCE_ELEMENTS);
		if(children.length == 0)
		{
			return JSSourceElement.EMPTY_ARRAY;
		}
		JSSourceElement[] result = new JSSourceElement[children.length];
		for(int i = 0; i < children.length; i++)
		{
			result[i] = (JSSourceElement) children[i].getPsi();
		}
		return result;
	}

	@Nonnull
	@Override
	public JavaScriptType getReturnType()
	{
		return JavaScriptType.UNKNOWN;
	}

	@Override
	public String getReturnTypeString()
	{
		return null;
	}

	@Nullable
	@Override
	public JavaScriptTypeElement getReturnTypeElement()
	{
		return null;
	}

	@Override
	public boolean isDeprecated()
	{
		return false;
	}

	@Nullable
	@Override
	public JSAttributeList getAttributeList()
	{
		return null;
	}

	@Override
	public String getQualifiedName()
	{
		return null;
	}
}

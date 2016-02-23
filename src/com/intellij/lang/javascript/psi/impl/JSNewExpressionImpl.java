/*
 * Copyright 2000-2005 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.intellij.lang.javascript.psi.impl;

import org.jetbrains.annotations.NotNull;
import org.mustbe.consulo.RequiredReadAction;
import org.mustbe.consulo.javascript.lang.psi.JavaScriptType;
import org.mustbe.consulo.javascript.lang.psi.impl.JavaScriptClassType;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSArgumentList;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSNewExpression;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 11:57:36 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSNewExpressionImpl extends JSExpressionImpl implements JSNewExpression
{
	public JSNewExpressionImpl(final ASTNode node)
	{
		super(node);
	}

	@RequiredReadAction
	@NotNull
	@Override
	public JavaScriptType getType()
	{
		JSExpression methodExpression = getMethodExpression();

		if(methodExpression instanceof JSReferenceExpression)
		{
			PsiElement resolvedElement = ((JSReferenceExpression) methodExpression).resolve();
			if(resolvedElement instanceof JSClass)
			{
				return new JavaScriptClassType((JSClass) resolvedElement);
			}
		}
		return super.getType();
	}

	@Override
	public JSExpression getMethodExpression()
	{
		final ASTNode node = getNode().findChildByType(JSElementTypes.EXPRESSIONS);
		return node != null ? (JSExpression) node.getPsi() : null;
	}

	@Override
	public JSArgumentList getArgumentList()
	{
		final ASTNode node = getNode().findChildByType(JSElementTypes.ARGUMENT_LIST);
		return node != null ? (JSArgumentList) node.getPsi() : null;
	}

	@Override
	public void accept(@NotNull PsiElementVisitor visitor)
	{
		if(visitor instanceof JSElementVisitor)
		{
			((JSElementVisitor) visitor).visitJSNewExpression(this);
		}
		else
		{
			visitor.visitElement(this);
		}
	}
}

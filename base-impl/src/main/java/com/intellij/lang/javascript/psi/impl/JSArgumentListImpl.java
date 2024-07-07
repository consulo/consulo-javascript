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

import com.intellij.lang.javascript.psi.JSArgumentList;
import com.intellij.lang.javascript.psi.JSCallExpression;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.language.ast.ASTNode;

import jakarta.annotation.Nonnull;

/**
 * User: max
 * Date: Jan 30, 2005
 * Time: 9:15:02 PM
 */
public class JSArgumentListImpl extends JSElementImpl implements JSArgumentList
{
	public JSArgumentListImpl(final ASTNode node)
	{
		super(node);
	}

	@Override
	public JSExpression[] getArguments()
	{
		return findChildrenByClass(JSExpression.class);
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSArgumentList(this);
	}

	@Override
	public void delete() throws IncorrectOperationException
	{
		PsiElement element = getParent();
		element.replace(((JSCallExpression) element).getMethodExpression());
	}
}

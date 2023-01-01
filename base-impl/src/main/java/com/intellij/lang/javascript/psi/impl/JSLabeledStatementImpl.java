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

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSLabeledStatement;
import com.intellij.lang.javascript.psi.JSNamedElement;
import com.intellij.lang.javascript.psi.JSStatement;
import consulo.language.util.IncorrectOperationException;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;

import javax.annotation.Nonnull;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 9:20:04 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSLabeledStatementImpl extends JSStatementImpl implements JSLabeledStatement, JSNamedElement
{
	public JSLabeledStatementImpl(final ASTNode node)
	{
		super(node);
	}

	@Override
	public String getLabel()
	{
		return getNameIdentifier().getText();
	}

	@Override
	public PsiElement getLabelIdentifier()
	{
		return getNameIdentifier();
	}

	@Override
	public JSStatement getStatement()
	{
		return findChildByClass(JSStatement.class);
	}

	@Override
	public JSStatement unlabel()
	{
		throw new UnsupportedOperationException("TODO: implement");
	}

	public JSLabeledStatement setLabel(String label)
	{
		throw new UnsupportedOperationException("TODO: implement");
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSLabeledStatement(this);
	}

	@Override
	public String getName()
	{
		return getLabel();
	}

	@Override
	public PsiElement setName(@Nonnull String name) throws IncorrectOperationException
	{
		JSChangeUtil.doIdentifierReplacement(this, getLabelIdentifier(), name);
		return this;
	}

	@Override
	public PsiElement getNameIdentifier()
	{
		return findChildByType(JSTokenTypes.IDENTIFIER);
	}
}

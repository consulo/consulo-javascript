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

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSImportStatement;
import com.intellij.lang.javascript.psi.stubs.JSImportStatementStub;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;

import javax.annotation.Nonnull;

/**
 * @by Maxim.Mossienko
 */
public class JSImportStatementImpl extends JSStubbedStatementImpl<JSImportStatementStub> implements JSImportStatement
{
	public JSImportStatementImpl(final ASTNode node)
	{
		super(node);
	}

	public JSImportStatementImpl(final JSImportStatementStub stub)
	{
		super(stub, JSElementTypes.ES4_IMPORT_STATEMENT);
	}

	@Override
	public void accept(@Nonnull PsiElementVisitor visitor)
	{
		if(visitor instanceof JSElementVisitor)
		{
			((JSElementVisitor) visitor).visitJSImportStatement(this);
		}
		else
		{
			visitor.visitElement(this);
		}
	}

	@Override
	public boolean processDeclarations(@Nonnull final PsiScopeProcessor processor, @Nonnull final ResolveState state, final PsiElement lastParent,
			@Nonnull final PsiElement place)
	{
		return true;
	}

	@Override
	public String getImportText()
	{
		final JSImportStatementStub stub = getStub();
		if(stub != null)
		{
			return stub.getImportText();
		}
		final ASTNode node = getNode().findChildByType(JSElementTypes.REFERENCE_EXPRESSION);
		return node != null ? node.getText() : null;
	}
}
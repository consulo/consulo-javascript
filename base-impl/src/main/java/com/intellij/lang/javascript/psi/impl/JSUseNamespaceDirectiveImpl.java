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

import consulo.language.ast.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSUseNamespaceDirective;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import com.intellij.lang.javascript.psi.stubs.JSUseNamespaceDirectiveStub;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;

import javax.annotation.Nonnull;

/**
 * @by Maxim.Mossienko
 */
public class JSUseNamespaceDirectiveImpl extends JSStubbedStatementImpl<JSUseNamespaceDirectiveStub> implements JSUseNamespaceDirective
{
	public JSUseNamespaceDirectiveImpl(final ASTNode node)
	{
		super(node);
	}

	public JSUseNamespaceDirectiveImpl(final JSUseNamespaceDirectiveStub stub)
	{
		super(stub, JSElementTypes.USE_NAMESPACE_DIRECTIVE);
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSUseNamespaceDirective(this);
	}

	@Override
	public String getNamespaceToBeUsed()
	{
		final JSUseNamespaceDirectiveStub stub = getStub();
		if(stub != null)
		{
			return stub.getNamespaceToUse();
		}
		final ASTNode node = getNode().findChildByType(JSElementTypes.REFERENCE_EXPRESSION);
		return node != null ? node.getText() : null;
	}

	@Override
	public boolean processDeclarations(@Nonnull PsiScopeProcessor processor, @Nonnull ResolveState state, PsiElement lastParent,
									   @Nonnull PsiElement place)
	{
		if(processor instanceof ResolveProcessor && ((ResolveProcessor) processor).lookingForUseNamespaces())
		{
			return processor.execute(this, state);
		}
		return true;
	}
}

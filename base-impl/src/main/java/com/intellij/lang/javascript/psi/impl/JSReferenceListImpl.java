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

import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSImportHandlingUtil;
import com.intellij.lang.javascript.psi.stubs.JSReferenceListStub;
import consulo.language.psi.PsiElement;
import consulo.language.psi.stub.IStubElementType;
import consulo.language.psi.stub.StubIndex;
import consulo.util.collection.ArrayUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.psi.stub.JavaScriptIndexKeys;
import consulo.language.ast.ASTNode;
import consulo.project.Project;
import org.jetbrains.annotations.NonNls;

import jakarta.annotation.Nonnull;
import java.util.ArrayList;
import java.util.Collection;

/**
 * @author Maxim.Mossienko
 */
public class JSReferenceListImpl extends JSStubElementImpl<JSReferenceListStub> implements JSReferenceList
{
	public JSReferenceListImpl(final ASTNode node)
	{
		super(node);
	}

	public JSReferenceListImpl(final JSReferenceListStub stub, IStubElementType stubElementType)
	{
		super(stub, stubElementType);
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSReferenceList(this);
	}

	@RequiredReadAction
	@Nonnull
	@Override
	public JSReferenceExpression[] getExpressions()
	{
		return findChildrenByClass(JSReferenceExpression.class);
	}

	@Nonnull
	@Override
	@RequiredReadAction
	public String[] getReferenceTexts()
	{
		final JSReferenceListStub stub = getStub();
		if(stub != null)
		{
			return stub.getReferenceTexts();
		}

		final JSReferenceExpression[] referenceExpressions = getExpressions();
		if(referenceExpressions.length == 0)
		{
			return ArrayUtil.EMPTY_STRING_ARRAY;
		}
		int count = referenceExpressions.length;
		final String[] result = ArrayUtil.newStringArray(count);

		for(int i = 0; i < count; ++i)
		{
			result[i] = referenceExpressions[i].getText();
		}
		return result;
	}

	@Nonnull
	@RequiredReadAction
	@Override
	public JSClass[] getReferencedClasses()
	{
		@NonNls String[] texts = getReferenceTexts();

		if(texts.length == 0)
		{
			return JSClass.EMPTY_ARRAY;
		}

		final Project project = getProject();
		final ArrayList<JSClass> supers = new ArrayList<JSClass>(1);

		for(String text : texts)
		{
			final int index = supers.size();

			text = JSImportHandlingUtil.resolveTypeName(text, this);

			final Collection<JSQualifiedNamedElement> candidates = StubIndex.getElements(JavaScriptIndexKeys.ELEMENTS_BY_QNAME, text, project, getResolveScope(), JSQualifiedNamedElement.class);
			for(JSQualifiedNamedElement _clazz : candidates)
			{
				if(!(_clazz instanceof JSClass))
				{
					continue;
				}
				final JSClass clazz = (JSClass) _clazz;

				if(text.equals(clazz.getQualifiedName()))
				{
					if(clazz.canNavigate())
					{
						supers.add(index, clazz);
					}
					else
					{
						supers.add(clazz);
					}
				}
			}

			if(candidates.size() == 0)
			{
				final PsiElement element = JSClassImpl.findClassFromNamespace(text, this);
				if(element instanceof JSClass)
				{
					supers.add((JSClass) element);
				}
			}
		}

		return supers.toArray(new JSClass[supers.size()]);
	}
}
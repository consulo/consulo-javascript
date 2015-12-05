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

import java.util.ArrayList;
import java.util.Collection;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSQualifiedNamedElement;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.JSReferenceList;
import com.intellij.lang.javascript.psi.resolve.JSImportHandlingUtil;
import com.intellij.lang.javascript.psi.stubs.JSQualifiedElementIndex;
import com.intellij.lang.javascript.psi.stubs.JSReferenceListStub;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.stubs.StubIndex;
import com.intellij.util.ArrayUtil;

/**
 * @by Maxim.Mossienko
 */
public class JSReferenceListImpl extends JSStubElementImpl<JSReferenceListStub> implements JSReferenceList
{
	public JSReferenceListImpl(final ASTNode node)
	{
		super(node);
	}

	public JSReferenceListImpl(final JSReferenceListStub stub)
	{
		super(stub, stub.getStubType());
	}

	@Override
	public void accept(@NotNull PsiElementVisitor visitor)
	{
		if(visitor instanceof JSElementVisitor)
		{
			((JSElementVisitor) visitor).visitJSReferenceList(this);
		}
		else
		{
			visitor.visitElement(this);
		}
	}

	@NotNull
	@Override
	public JSReferenceExpression[] getExpressions()
	{
		return findChildrenByClass(JSReferenceExpression.class);
	}

	@Override
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

			final Collection<JSQualifiedNamedElement> candidates = StubIndex.getElements(JSQualifiedElementIndex.KEY, text, project, getResolveScope(), JSQualifiedNamedElement.class);
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
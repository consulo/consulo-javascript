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
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.ResolveState;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.IncorrectOperationException;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 11:24:42 PM
 */
public class JSSuperExpressionImpl extends JSExpressionImpl implements JSSuperExpression
{
	public JSSuperExpressionImpl(final ASTNode node)
	{
		super(node);
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSSuperExpression(this);
	}

	@Override
	@Nonnull
	public PsiReference[] getReferences()
	{
		PsiReference[] refs = {
				new PsiReference()
				{
					@Override
					public PsiElement getElement()
					{
						return JSSuperExpressionImpl.this;
					}

					@Override
					public TextRange getRangeInElement()
					{
						return new TextRange(0, getTextLength());
					}

					@Override
					@Nullable
					public PsiElement resolve()
					{
						final PsiElement element = findClass();

						if(getElement().getParent() instanceof JSCallExpression && element instanceof JSClass)
						{
							final JSClass clazz = (JSClass) element;
							final ResolveProcessor processor = new ResolveProcessor(clazz.getName(), JSSuperExpressionImpl.this);
							element.processDeclarations(processor, ResolveState.initial(), clazz, getElement());
							if(processor.getResult() != null)
							{
								return processor.getResult();
							}
						}

						return element;
					}

					private PsiElement findClass()
					{
						final JSClass jsClass = PsiTreeUtil.getParentOfType(getElement(), JSClass.class);

						if(jsClass != null)
						{
							final JSReferenceList extendsList = jsClass.getExtendsList();
							if(extendsList != null)
							{
								final JSReferenceExpression[] referenceExpressions = extendsList.getExpressions();
								if(referenceExpressions != null && referenceExpressions.length > 0)
								{
									final ResolveResult[] results = referenceExpressions[0].multiResolve(false);
									return results.length > 0 ? results[0].getElement() : null;
								}
							}
						}
						else
						{
							final JSFile jsFile = PsiTreeUtil.getParentOfType(getElement(), JSFile.class);

							if(jsFile != null)
							{
								return JSResolveUtil.getClassReferenceForXmlFromContext(jsFile);
							}
						}
						return null;
					}

					@Override
					public String getCanonicalText()
					{
						return getText();
					}

					@Override
					public PsiElement handleElementRename(final String newElementName) throws IncorrectOperationException
					{
						return null;
					}

					@Override
					public PsiElement bindToElement(@Nonnull final PsiElement element) throws IncorrectOperationException
					{
						return null;
					}

					@Override
					public boolean isReferenceTo(final PsiElement element)
					{
						return false;
					}

					@Override
					public Object[] getVariants()
					{
						return ArrayUtil.EMPTY_OBJECT_ARRAY;
					}

					@Override
					public boolean isSoft()
					{
						return true;
					}
				}
		};
		return refs;
	}
}
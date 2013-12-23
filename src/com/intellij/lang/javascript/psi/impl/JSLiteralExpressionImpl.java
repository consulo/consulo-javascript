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
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiReference;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 11:24:42 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSLiteralExpressionImpl extends JSExpressionImpl implements JSLiteralExpression
{
	private volatile JSReferenceSet myReferenceSet;
	private volatile long myModCount;

	public JSLiteralExpressionImpl(final ASTNode node)
	{
		super(node);
	}

	@Override
	@NotNull
	public PsiReference[] getReferences()
	{
		JSReferenceSet referenceSet = myReferenceSet;

		if(referenceSet == null)
		{
			synchronized(this)
			{
				referenceSet = myReferenceSet;
				if(referenceSet == null)
				{
					referenceSet = new JSReferenceSet(this);
					referenceSet.update(getText(), 0);
					myReferenceSet = referenceSet;
				}
			}
		}
		else
		{
			final long count = getManager().getModificationTracker().getModificationCount();

			if(count != myModCount)
			{
				synchronized(this)
				{
					if(count != myModCount)
					{
						referenceSet.update(getText(), 0);
						myModCount = count;
					}
				}
			}
		}

		return myReferenceSet.getReferences();
	}

	@Override
	public void accept(@NotNull PsiElementVisitor visitor)
	{
		if(visitor instanceof JSElementVisitor)
		{
			((JSElementVisitor) visitor).visitJSLiteralExpression(this);
		}
		else
		{
			visitor.visitElement(this);
		}
	}

}

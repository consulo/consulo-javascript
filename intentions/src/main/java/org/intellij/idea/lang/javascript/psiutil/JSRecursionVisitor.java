/*
 * Copyright 2005-2006 Olivier Descout
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
package org.intellij.idea.lang.javascript.psiutil;

import com.intellij.lang.javascript.psi.*;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiReference;

public class JSRecursionVisitor extends JSRecursiveElementVisitor
{

	private final JSFunction function;
	private final String functionName;
	private boolean recursive;

	public JSRecursionVisitor(JSFunction function)
	{
		this.function = function;
		this.functionName = function.getName();
	}

	@Override
	public void visitJSElement(JSElement element)
	{
		if(!this.recursive)
		{
			super.visitJSElement(element);
		}
	}

	@Override
	public void visitJSCallExpression(JSCallExpression call)
	{
		if(!this.recursive)
		{
			super.visitJSCallExpression(call);

			final JSExpression methodExpression = call.getMethodExpression();
			final String qualifiedMethodText = methodExpression.getText();
			final String methodText = qualifiedMethodText.substring(qualifiedMethodText.lastIndexOf('.') + 1);

			if(methodText.equals(this.functionName))
			{
				final PsiReference methodReference = methodExpression.getReference();
				final PsiElement referent = ((methodReference == null) ? null : methodReference.resolve());

				if(referent != null)
				{
					if(referent instanceof JSFunction)
					{
						this.recursive = referent.equals(this.function);
					}
					else if(referent instanceof JSFunctionExpression)
					{
						this.recursive = ((JSFunctionExpression) referent).getFunction().equals(this.function);
					}
				}
			}
		}
	}

	public boolean isFunctionNamed()
	{
		return (this.functionName != null);
	}

	public boolean isRecursive()
	{
		return this.recursive;
	}
}

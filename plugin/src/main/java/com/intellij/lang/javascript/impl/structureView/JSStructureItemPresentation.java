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

package com.intellij.lang.javascript.impl.structureView;

import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.icon.IconDescriptorUpdaters;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiNamedElement;
import consulo.ui.image.Image;

import javax.annotation.Nonnull;

/**
 * @author Maxim.Mossienko
 *         Date: Jul 23, 2008
 *         Time: 6:54:27 PM
 */
public class JSStructureItemPresentation extends JSStructureViewElement.JSStructureItemPresentationBase
{
	public JSStructureItemPresentation(final JSStructureViewElement jsStructureViewElement)
	{
		super(jsStructureViewElement);
	}

	@Override
	@RequiredReadAction
	public String getPresentableText()
	{
		PsiElement psiElement = element.getUpToDateElement();
		if(psiElement == null || !psiElement.isValid())
		{
			return "*invalid*";
		}

		return getName(psiElement);
	}

	@RequiredReadAction
	public static String getName(@Nonnull PsiElement psiElement)
	{
		if(psiElement instanceof JSObjectLiteralExpression)
		{
			if (psiElement.getParent() instanceof JSAssignmentExpression assignmentExpression)
			{
				final JSExpression expression = ((JSDefinitionExpression) assignmentExpression.getLOperand()).getExpression();
				return JSResolveUtil.findClassIdentifier(expression).getText();
			}
			else
			{
				return JavaScriptLocalize.javascriptLanguageTermPrototype().get();
			}
		}

		if (psiElement instanceof JSDefinitionExpression definitionExpression)
		{
			psiElement = definitionExpression.getExpression();
		}

		if (psiElement instanceof JSReferenceExpression expression)
		{
			String s = expression.getReferencedName();

			if(JSResolveUtil.PROTOTYPE_FIELD_NAME.equals(s))
			{
				final JSExpression jsExpression = expression.getQualifier();
				if (jsExpression instanceof JSReferenceExpression referenceExpression)
				{
					s = referenceExpression.getReferencedName();
				}
			}
			return s;
		}

		if(!(psiElement instanceof PsiNamedElement))
		{
			return psiElement.getText();
		}

		String name = ((PsiNamedElement) psiElement).getName();

		if (psiElement instanceof JSProperty property)
		{
			psiElement = property.getValue();
		}

		if(psiElement instanceof JSFunction)
		{
			if(name == null)
			{
				name = "<anonymous>";
			}
			name += "(";
			JSParameterList parameterList = ((JSFunction) psiElement).getParameterList();
			if(parameterList != null)
			{
				for(JSParameter p : parameterList.getParameters())
				{
					if(!name.endsWith("("))
					{
						name += ", ";
					}
					name += p.getName();
					final String variableType = p.getTypeString();
					if(variableType != null)
					{
						name += ":" + variableType;
					}
				}
			}
			name += ")";

			final String type = ((JSFunction) psiElement).getReturnTypeString();
			if(type != null)
			{
				name += ":" + type;
			}
		}

		if (name == null && psiElement.getParent() instanceof JSAssignmentExpression assignmentExpression)
		{
			JSExpression lOperand = ((JSDefinitionExpression) assignmentExpression.getLOperand()).getExpression();
			lOperand = JSResolveUtil.findClassIdentifier(lOperand);
			if (lOperand instanceof JSReferenceExpression referenceExpression)
			{
				return referenceExpression.getReferencedName();
			}
			return lOperand.getText();
		}
		return name;
	}

	@Override
	@RequiredReadAction
	public Image getIcon()
	{
		final PsiElement psiElement = this.element.getRealElement();
		if(!psiElement.isValid())
		{
			return null;
		}
		return IconDescriptorUpdaters.getIcon(psiElement, 0);
	}
}

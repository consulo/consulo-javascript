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

package com.intellij.lang.javascript.generation;

import com.intellij.codeInsight.generation.ClassMember;
import com.intellij.codeInsight.generation.MemberChooserObject;
import com.intellij.codeInsight.generation.PsiElementMemberChooserObject;
import com.intellij.icons.AllIcons;
import com.intellij.javascript.JSParameterInfoHandler;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSNamedElement;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSParameterList;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.psi.PsiElement;
import consulo.awt.TargetAWT;
import consulo.ide.IconDescriptorUpdaters;
import consulo.ui.image.Image;
import consulo.ui.image.ImageEffects;

/**
 * @author Maxim.Mossienko
 * Date: Jul 17, 2008
 * Time: 8:55:57 PM
 */
public class JSNamedElementNode extends PsiElementMemberChooserObject implements ClassMember
{
	public JSNamedElementNode(JSNamedElement node)
	{
		super(node, buildTextFor(node), TargetAWT.to(buildIcon(node)));
	}

	private static Image buildIcon(final JSNamedElement node)
	{
		Image icon = IconDescriptorUpdaters.getIcon(node, 0);

		if(node instanceof JSFunction)
		{
			final JSFunction function = (JSFunction) node;
			final Image accessIcon;

			if(function.isGetProperty())
			{
				accessIcon = AllIcons.Nodes.Read_access;
			}
			else if(function.isSetProperty())
			{
				accessIcon = AllIcons.Nodes.Write_access;
			}
			else
			{
				accessIcon = null;
			}

			if(accessIcon != null)
			{
				icon = ImageEffects.appendRight(icon, accessIcon);
			}
		}
		return icon;
	}

	private static String buildTextFor(final JSNamedElement node)
	{
		String text = node.getName();

		if(node instanceof JSFunction)
		{
			final JSFunction function = (JSFunction) node;
			text += "(";
			final JSParameterList parameterList = function.getParameterList();

			if(parameterList != null)
			{
				boolean first = true;
				for(JSParameter p : parameterList.getParameters())
				{
					if(!first)
					{
						text += ", ";
					}
					first = false;
					text += JSParameterInfoHandler.getSignatureForParameter(p, false);
				}
			}

			text += ")";
			final String typeString = function.getReturnTypeString();
			if(typeString != null)
			{
				text += ":" + typeString;
			}
		}
		else if(node instanceof JSVariable)
		{
			final JSVariable var = (JSVariable) node;
			final String typeString = var.getTypeString();
			if(typeString != null)
			{
				text += ":" + typeString;
			}
		}
		return text;
	}

	@Override
	public MemberChooserObject getParentNodeDelegate()
	{
		final PsiElement element = getPsiElement();
		PsiElement parent = JSResolveUtil.findParent(element);
		return new JSNamedElementNode((JSNamedElement) parent);
	}
}

/*
 * Copyright 2013-2015 must-be.org
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

package org.mustbe.consulo.javascript.ide;

import javax.swing.Icon;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.RequiredReadAction;
import com.intellij.icons.AllIcons;
import com.intellij.ide.IconDescriptor;
import com.intellij.ide.IconDescriptorUpdater;
import com.intellij.ide.IconDescriptorUpdaters;
import com.intellij.lang.javascript.index.JSNamedElementProxy;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.VariantsProcessor;
import com.intellij.openapi.util.Iconable;
import com.intellij.psi.PsiElement;
import com.intellij.util.BitUtil;

/**
 * @author VISTALL
 * @since 05.12.2015
 */
public class JavaScriptIconDescriptorUpdater implements IconDescriptorUpdater
{
	@RequiredReadAction
	@Override
	public void updateIcon(@NotNull IconDescriptor iconDescriptor, @NotNull PsiElement element, int flags)
	{
		if(element instanceof VariantsProcessor.MyElementWrapper)
		{
			String artificialName = ((VariantsProcessor.MyElementWrapper) element).getArtificialName();
			if(artificialName == null)
			{
				return;
			}
			JSNamedElementProxy proxy = ((VariantsProcessor.MyElementWrapper) element).getProxy();
			if(proxy != null)
			{
				IconDescriptorUpdaters.processExistingDescriptor(iconDescriptor, proxy, flags);
			}
			else
			{
				iconDescriptor.setMainIcon(AllIcons.Nodes.Class);
			}
		}
		else if(element instanceof JSProperty)
		{
			iconDescriptor.setMainIcon(AllIcons.Nodes.Property);
			JSExpression value = ((JSProperty) element).getValue();
			if(value instanceof JSFunctionExpression)
			{
				iconDescriptor.setMainIcon(AllIcons.Nodes.Function);
			}

			if(BitUtil.isSet(flags, Iconable.ICON_FLAG_VISIBILITY))
			{
				iconDescriptor.setRightIcon(AllIcons.Nodes.C_public);
			}
		}
		else if(element instanceof JSNamedElementProxy)
		{
			iconDescriptor.setMainIcon(getIconForProxy((JSNamedElementProxy) element));
			iconDescriptor.setRightIcon(getAccessIcon(((JSNamedElementProxy) element).getAccessType()));
		}
		else if(element instanceof JSClass)
		{
			final JSAttributeList attributeList = ((JSClass) element).getAttributeList();

			iconDescriptor.setMainIcon(((JSClass) element).isInterface() ? AllIcons.Nodes.Interface : AllIcons.Nodes.Class);
			addStaticAndFinalIcons(iconDescriptor, attributeList);

			if(BitUtil.isSet(flags, Iconable.ICON_FLAG_VISIBILITY))
			{
				final JSAttributeList.AccessType accessType = attributeList != null ? attributeList.getAccessType() : JSAttributeList.AccessType.PACKAGE_LOCAL;
				iconDescriptor.setRightIcon(getAccessIcon(accessType));
			}
		}
		else if(element instanceof JSParameter)
		{
			iconDescriptor.setMainIcon(AllIcons.Nodes.Parameter);
			if(BitUtil.isSet(flags, Iconable.ICON_FLAG_VISIBILITY))
			{
				iconDescriptor.setRightIcon(AllIcons.Nodes.C_plocal);
			}
		}
		else if(element instanceof JSVariable)
		{
			iconDescriptor.setMainIcon(AllIcons.Nodes.Variable);

			final PsiElement grandParent = JSResolveUtil.findParent(element);

			if(grandParent instanceof JSClass)
			{
				final JSAttributeList attributeList = ((JSVariable) element).getAttributeList();
				if(attributeList != null)
				{
					addStaticAndFinalIcons(iconDescriptor, attributeList);

					if(BitUtil.isSet(flags, Iconable.ICON_FLAG_VISIBILITY))
					{
						iconDescriptor.setRightIcon(getAccessIcon(attributeList.getAccessType()));
					}
				}
				else
				{
					if(BitUtil.isSet(flags, Iconable.ICON_FLAG_VISIBILITY))
					{
						iconDescriptor.setRightIcon(getAccessIcon(JSAttributeList.AccessType.PUBLIC));
					}
				}
			}
			else if(grandParent instanceof JSBlockStatement || grandParent instanceof JSLoopStatement)
			{
				if(BitUtil.isSet(flags, Iconable.ICON_FLAG_VISIBILITY))
				{
					iconDescriptor.setRightIcon(AllIcons.Nodes.C_private);
				}
			}
			else
			{
				if(BitUtil.isSet(flags, Iconable.ICON_FLAG_VISIBILITY))
				{
					iconDescriptor.setRightIcon(AllIcons.Nodes.C_public);
				}
			}
		}
		else if(element instanceof JSNamespaceDeclaration)
		{
			iconDescriptor.setMainIcon(AllIcons.Nodes.Package);
		}
		else if(element instanceof JSFunction)
		{
			iconDescriptor.setMainIcon(AllIcons.Nodes.Function);

			final PsiElement parent = JSResolveUtil.findParent(element);
			if(parent instanceof JSBlockStatement)
			{
				if(BitUtil.isSet(flags, Iconable.ICON_FLAG_VISIBILITY))
				{
					iconDescriptor.setRightIcon(AllIcons.Nodes.C_private);
				}
			}
			else if(parent instanceof JSClass)
			{
				final JSAttributeList attributeList = ((JSFunction) element).getAttributeList();
				if(attributeList != null)
				{
					addStaticAndFinalIcons(iconDescriptor, attributeList);
					if(BitUtil.isSet(flags, Iconable.ICON_FLAG_VISIBILITY))
					{
						iconDescriptor.setRightIcon(getAccessIcon(attributeList.getAccessType()));
					}
				}
				else
				{
					if(BitUtil.isSet(flags, Iconable.ICON_FLAG_VISIBILITY))
					{
						iconDescriptor.setRightIcon(getAccessIcon(JSAttributeList.AccessType.PUBLIC));
					}
				}
			}
			else
			{
				if(BitUtil.isSet(flags, Iconable.ICON_FLAG_VISIBILITY))
				{
					iconDescriptor.setRightIcon(getAccessIcon(JSAttributeList.AccessType.PUBLIC));
				}
			}
		}
	}

	@NotNull
	private static Icon getIconForProxy(JSNamedElementProxy proxy)
	{
		final JSNamedElementProxy.NamedItemType valueType = proxy.getType();

		if(valueType == JSNamedElementProxy.NamedItemType.Function ||
				valueType == JSNamedElementProxy.NamedItemType.FunctionExpression ||
				valueType == JSNamedElementProxy.NamedItemType.MemberFunction ||
				valueType == JSNamedElementProxy.NamedItemType.ImplicitFunction ||
				valueType == JSNamedElementProxy.NamedItemType.FunctionProperty)
		{
			if(valueType == JSNamedElementProxy.NamedItemType.MemberFunction && (proxy.hasProperty(JSNamedElementProxy.Property.GetFunction) || proxy.hasProperty(JSNamedElementProxy.Property
					.SetFunction)))
			{
				return AllIcons.Nodes.Property;
			}
			return AllIcons.Nodes.Function;
		}

		if(valueType == JSNamedElementProxy.NamedItemType.Variable ||
				valueType == JSNamedElementProxy.NamedItemType.MemberVariable ||
				valueType == JSNamedElementProxy.NamedItemType.ImplicitVariable)
		{
			return AllIcons.Nodes.Variable;
		}
		if(valueType == JSNamedElementProxy.NamedItemType.Property)
		{
			return AllIcons.Nodes.Property;
		}
		if(valueType == JSNamedElementProxy.NamedItemType.AttributeValue)
		{
			return AllIcons.Nodes.Tag;
		}
		if(valueType == JSNamedElementProxy.NamedItemType.Clazz || valueType == JSNamedElementProxy.NamedItemType.Namespace)
		{
			return proxy.hasProperty(JSNamedElementProxy.Property.Interface) ? AllIcons.Nodes.Interface : AllIcons.Nodes.Class;
		}

		return AllIcons.Nodes.Variable;
	}

	@Nullable
	public Icon getAccessIcon(JSAttributeList.AccessType accessType)
	{
		switch(accessType)
		{
			case PUBLIC:
				return AllIcons.Nodes.C_public;
			case PRIVATE:
				return AllIcons.Nodes.C_private;
			case PROTECTED:
				return AllIcons.Nodes.C_protected;
			case PACKAGE_LOCAL:
				return AllIcons.Nodes.C_plocal;
		}
		return null;
	}

	private static void addStaticAndFinalIcons(IconDescriptor iconDescriptor, JSAttributeList attrList)
	{
		if(attrList == null)
		{
			return;
		}
		if(attrList.hasModifier(JSAttributeList.ModifierType.STATIC) || attrList.hasModifier(JSAttributeList.ModifierType.DYNAMIC))   // dynamic?
		{
			iconDescriptor.addLayerIcon(AllIcons.Nodes.StaticMark);
		}
		if(attrList.hasModifier(JSAttributeList.ModifierType.FINAL))
		{
			iconDescriptor.addLayerIcon(AllIcons.Nodes.FinalMark);
		}
	}
}

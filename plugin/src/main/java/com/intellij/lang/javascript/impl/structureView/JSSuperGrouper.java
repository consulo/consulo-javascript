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

import consulo.fileEditor.structureView.tree.*;
import consulo.project.ui.view.tree.AbstractTreeNode;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSVarStatement;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.language.psi.PsiElement;
import consulo.ide.IdeBundle;
import consulo.platform.base.icon.PlatformIconGroup;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Maxim.Mossienko
 */
class JSSuperGrouper implements Grouper
{
	@NonNls
	private static final String SHOW_CLASSES = "SHOW_CLASSES";

	@Override
	@Nonnull
	public Collection<Group> group(final Object parent, final Collection<TreeElement> children)
	{
		if(isParentGrouped((AbstractTreeNode) parent))
		{
			return Collections.emptyList();
		}
		final Map<String, Group> groups = new HashMap<String, Group>();

		for(TreeElement _child : children)
		{
			if(!(_child instanceof JSStructureViewElement))
			{
				continue;
			}
			JSStructureViewElement child = (JSStructureViewElement) _child;
			final PsiElement value = child.getValue();

			if(value instanceof JSVariable)
			{
				if(!child.isInherited())
				{
					continue;
				}
				PsiElement parentElement = value.getParent();
				if(parentElement instanceof JSVarStatement)
				{
					parentElement = parentElement.getParent();
				}
				if(parentElement instanceof JSClass)
				{
					addGroup(groups, _child, ((JSClass) parentElement).getQualifiedName());
				}
			}
			else if(value instanceof JSFunction)
			{
				processFunction((JSStructureViewElement) ((AbstractTreeNode) parent).getValue(), groups, _child, value);
			}
		}
		return groups.values();
	}

	private static void processFunction(JSStructureViewElement parentElement, Map<String, Group> groups, TreeElement _child, PsiElement value)
	{
		final PsiElement element = JSStructureViewElement.getPsiElementResolveProxy(parentElement);
		if(element instanceof JSClass)
		{
			JSClass parentClass = (JSClass) element;
			JSClass declaringClass = JSResolveUtil.findDeclaringClass((JSFunction) value);
			if(parentClass != declaringClass)
			{
				addGroup(groups, _child, declaringClass.getQualifiedName());
			}
		}
	}

	private static void addGroup(final Map<String, Group> groups, final TreeElement _child, final String qName)
	{
		JSSuperGroup group;
		if((group = ((JSSuperGroup) groups.get(qName))) == null)
		{
			groups.put(qName, group = new JSSuperGroup(qName));
		}

		group.addChild(_child);
	}

	@Override
	@Nonnull
	public ActionPresentation getPresentation()
	{
		return new ActionPresentationData(IdeBundle.message("action.structureview.group.methods.by.defining.type"), null, PlatformIconGroup.generalImplementingmethod());
	}

	@Override
	@Nonnull
	public String getName()
	{
		return SHOW_CLASSES;
	}

	private static boolean isParentGrouped(AbstractTreeNode parent)
	{
		while(parent != null)
		{
			if(parent.getValue() instanceof JSSuperGroup)
			{
				return true;
			}
			parent = (AbstractTreeNode) parent.getParent();
		}
		return false;
	}
}

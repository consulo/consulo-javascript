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

package com.intellij.lang.javascript.findUsages;

import javax.swing.Icon;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.JSNamedElement;
import com.intellij.navigation.NavigationItem;
import com.intellij.openapi.actionSystem.DataKey;
import com.intellij.openapi.actionSystem.DataSink;
import com.intellij.openapi.actionSystem.LangDataKeys;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Comparing;
import com.intellij.openapi.vcs.FileStatus;
import com.intellij.openapi.vcs.FileStatusManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.SmartPointerManager;
import com.intellij.psi.SmartPsiElementPointer;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.usageView.UsageInfo;
import com.intellij.usages.Usage;
import com.intellij.usages.UsageGroup;
import com.intellij.usages.UsageView;
import com.intellij.usages.impl.FileStructureGroupRuleProvider;
import com.intellij.usages.rules.PsiElementUsage;
import com.intellij.usages.rules.UsageGroupingRule;

/**
 * @author Maxim.Mossienko
 */
abstract class JavaScriptGroupRuleProviderBase<T extends JSNamedElement> implements FileStructureGroupRuleProvider
{
	@Override
	@Nullable
	public UsageGroupingRule getUsageGroupingRule(final Project project)
	{
		return new UsageGroupingRule()
		{
			@Override
			@Nullable
			public UsageGroup groupUsage(final Usage usage)
			{
				if(usage instanceof PsiElementUsage)
				{
					PsiElement psiElement = ((PsiElementUsage) usage).getElement();

					if(!psiElement.getLanguage().isKindOf(JavaScriptSupportLoader.JAVASCRIPT.getLanguage()))
					{
						return null;
					}
					final JSNamedElement element = PsiTreeUtil.getParentOfType(psiElement, getUsageClass());

					if(isAcceptableElement(element))
					{
						return createUsageGroup((T) element);
					}
				}
				return null;
			}
		};
	}

	protected boolean isAcceptableElement(JSNamedElement element)
	{
		return element != null;
	}

	protected abstract Class<? extends JSNamedElement> getUsageClass();

	protected abstract UsageGroup createUsageGroup(final T t);

	/**
	 * @author Maxim.Mossienko
	 */
	abstract static class PsiNamedElementUsageGroupBase<T extends PsiNamedElement & NavigationItem> implements UsageGroup
	{
		private SmartPsiElementPointer myElementPointer;
		private String myName;
		private Icon myIcon;

		PsiNamedElementUsageGroupBase(@NotNull T element, Icon icon)
		{
			myIcon = icon;

			myName = element.getName();
			if(myName == null)
			{
				myName = "<anonymous>";
			}
			myElementPointer = SmartPointerManager.getInstance(element.getProject()).createLazyPointer(element);
		}

		@Override
		public Icon getIcon(boolean isOpen)
		{
			return myIcon;
		}

		public T getElement()
		{
			return (T) myElementPointer.getElement();
		}

		@Override
		@NotNull
		public String getText(UsageView view)
		{
			return myName;
		}

		@Override
		public FileStatus getFileStatus()
		{
			return isValid() ? FileStatusManager.getInstance(getElement().getProject()).getStatus(getElement().getContainingFile().getVirtualFile()) : null;
		}

		@Override
		public boolean isValid()
		{
			final T element = getElement();
			return element != null && element.isValid();
		}

		@Override
		public void navigate(boolean focus) throws UnsupportedOperationException
		{
			if(canNavigate())
			{
				getElement().navigate(focus);
			}
		}

		@Override
		public boolean canNavigate()
		{
			return isValid();
		}

		@Override
		public boolean canNavigateToSource()
		{
			return canNavigate();
		}

		@Override
		public void update()
		{
		}

		@Override
		public int compareTo(final UsageGroup o)
		{
			return myName.compareTo(((PsiNamedElementUsageGroupBase) o).myName);
		}

		@Override
		public boolean equals(final Object obj)
		{
			if(!(obj instanceof PsiNamedElementUsageGroupBase))
			{
				return false;
			}
			PsiNamedElementUsageGroupBase group = (PsiNamedElementUsageGroupBase) obj;
			if(isValid() && group.isValid())
			{
				return getElement().getManager().areElementsEquivalent(getElement(), group.getElement());
			}
			return Comparing.equal(myName, ((PsiNamedElementUsageGroupBase) obj).myName);
		}

		@Override
		public int hashCode()
		{
			return myName.hashCode();
		}

		public void calcData(final DataKey key, final DataSink sink)
		{
			if(!isValid())
			{
				return;
			}
			if(LangDataKeys.PSI_ELEMENT == key)
			{
				sink.put(LangDataKeys.PSI_ELEMENT, getElement());
			}
			if(UsageView.USAGE_INFO_KEY == key)
			{
				T element = getElement();
				if(element != null)
				{
					sink.put(UsageView.USAGE_INFO_KEY, new UsageInfo(element));
				}
			}
		}
	}
}
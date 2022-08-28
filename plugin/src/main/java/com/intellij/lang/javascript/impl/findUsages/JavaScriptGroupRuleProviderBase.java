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

package com.intellij.lang.javascript.impl.findUsages;

import com.intellij.lang.javascript.psi.JSNamedElement;
import consulo.dataContext.DataSink;
import consulo.dataContext.TypeSafeDataProvider;
import consulo.language.editor.LangDataKeys;
import consulo.language.psi.PsiElement;
import consulo.language.psi.SmartPointerManager;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.project.Project;
import consulo.usage.*;
import consulo.usage.rule.PsiElementUsage;
import consulo.usage.rule.UsageGroupingRule;
import consulo.virtualFileSystem.status.FileStatusManager;
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.SmartPsiElementPointer;
import consulo.usage.UsageGroup;
import consulo.usage.UsageView;
import consulo.usage.rule.FileStructureGroupRuleProvider;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.navigation.NavigationItem;
import consulo.ui.image.Image;
import consulo.util.dataholder.Key;
import consulo.util.lang.Comparing;
import consulo.virtualFileSystem.status.FileStatus;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

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

					if(!psiElement.getLanguage().isKindOf(JavaScriptLanguage.INSTANCE))
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
	abstract static class PsiNamedElementUsageGroupBase<T extends PsiNamedElement & NavigationItem> implements UsageGroup, TypeSafeDataProvider
	{
		private SmartPsiElementPointer myElementPointer;
		private String myName;
		private Image myIcon;

		PsiNamedElementUsageGroupBase(@Nonnull T element, Image icon)
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
		public Image getIcon()
		{
			return myIcon;
		}

		public T getElement()
		{
			return (T) myElementPointer.getElement();
		}

		@Override
		@Nonnull
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

		@Override
		public void calcData(final Key<?> key, final DataSink sink)
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
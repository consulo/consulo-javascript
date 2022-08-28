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

import consulo.javascript.language.JavaScriptBundle;
import com.intellij.lang.javascript.psi.*;
import consulo.language.cacheBuilder.WordsScanner;
import consulo.language.findUsage.FindUsagesProvider;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiNamedElement;
import consulo.xml.psi.xml.XmlTag;
import consulo.xml.psi.xml.XmlToken;

import javax.annotation.Nonnull;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Feb 14, 2005
 * Time: 6:44:02 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class JavaScriptFindUsagesProvider implements FindUsagesProvider
{
	@Override
	public boolean canFindUsagesFor(@Nonnull PsiElement psiElement)
	{
		return psiElement instanceof PsiNamedElement;
	}

	@Override
	public String getHelpId(@Nonnull PsiElement psiElement)
	{
		return null;
	}

	@Override
	@Nonnull
	public String getType(@Nonnull PsiElement element)
	{
		if(element instanceof JSFunction)
		{
			return JavaScriptBundle.message("javascript.language.term.function");
		}
		if(element instanceof JSClass)
		{
			return JavaScriptBundle.message("javascript.language.term.class");
		}
		if(element instanceof JSNamespaceDeclaration)
		{
			return JavaScriptBundle.message("javascript.language.term.namespace");
		}
		if(element instanceof JSParameter)
		{
			return JavaScriptBundle.message("javascript.language.term.parameter");
		}
		if(element instanceof JSProperty)
		{
			return JavaScriptBundle.message("javascript.language.term.property");
		}
		if(element instanceof JSVariable)
		{
			return JavaScriptBundle.message("javascript.language.term.variable");
		}
		if(element instanceof JSLabeledStatement)
		{
			return JavaScriptBundle.message("javascript.language.term.label");
		}
		if(element instanceof JSDefinitionExpression)
		{
			return JavaScriptBundle.message("javascript.language.term.value");
		}
		if(element instanceof XmlTag)
		{
			return JavaScriptBundle.message("javascript.language.term.tag");
		}
		if(element instanceof XmlToken)
		{
			return JavaScriptBundle.message("javascript.language.term.attribute.value");
		}
		if(element instanceof JSPackageStatement)
		{
			return JavaScriptBundle.message("javascript.language.term.package");
		}
		return "";
	}

	@Override
	@Nonnull
	public String getDescriptiveName(@Nonnull PsiElement element)
	{
		String name = ((PsiNamedElement) element).getName();
		return name != null ? name : "";
	}

	@Override
	@Nonnull
	public String getNodeText(@Nonnull PsiElement element, boolean useFullName)
	{
		return getDescriptiveName(element);
	}

	@Override
	public WordsScanner getWordsScanner()
	{
		return new JSWordsScanner();
	}
}

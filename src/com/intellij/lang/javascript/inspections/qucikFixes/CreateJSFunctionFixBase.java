/*
 * Copyright 2000-2005 JetBrains s.r.o
 * Copyright 2013-2016 must-be.org
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

package com.intellij.lang.javascript.inspections.qucikFixes;

import java.util.Set;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.PropertyKey;
import org.mustbe.consulo.RequiredReadAction;
import org.mustbe.consulo.javascript.lang.JavaScriptFeature;
import com.intellij.codeInsight.template.Template;
import com.intellij.lang.javascript.JavaScriptBundle;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;

/**
* @author VISTALL
* @since 24.02.2016
*/
public abstract class CreateJSFunctionFixBase extends BaseCreateFix
{
	private final String myName;
	private final String myIntentionNameKey;

	public CreateJSFunctionFixBase(String name, @PropertyKey(resourceBundle = JavaScriptBundle.BUNDLE) String nameKey)
	{
		myName = name;
		myIntentionNameKey = nameKey;
	}

	@Override
	@NotNull
	public String getName()
	{
		return JavaScriptBundle.message(myIntentionNameKey, myName);
	}

	@Override
	@NotNull
	public String getFamilyName()
	{
		return JavaScriptBundle.message("javascript.create.function.intention.family");
	}

	@RequiredReadAction
	@Override
	protected void buildTemplate(Template template, JSReferenceExpression referenceExpression, Set<JavaScriptFeature> features, boolean staticContext, PsiFile file,
			PsiElement anchorParent)
	{
		boolean classFeature = features.contains(JavaScriptFeature.CLASS);
		String referencedName = classFeature ? referenceExpression.getReferencedName() : referenceExpression.getText();
		addAccessModifier(template, referenceExpression, classFeature, staticContext);
		writeFunctionAndName(template, referencedName, features);
		template.addTextSegment("(");

		addParameters(template, referenceExpression, file, features);

		template.addTextSegment(")");

		if(classFeature)
		{
			template.addTextSegment(":");
			addReturnType(template, referenceExpression, file);
		}

		JSClass clazz = findClass(file, anchorParent);
		if(clazz == null || !clazz.isInterface())
		{
			template.addTextSegment(" {");
			addBody(template, referenceExpression, file);
			template.addTextSegment("}");
		}
		else
		{
			addSemicolonSegment(template, file);
			template.addEndVariable();
		}
	}

	protected void writeFunctionAndName(Template template, String referencedName, Set<JavaScriptFeature> features)
	{
		template.addTextSegment("function ");
		template.addTextSegment(referencedName);
	}

	protected abstract void addParameters(Template template, JSReferenceExpression refExpr, PsiFile file, Set<JavaScriptFeature> features);

	protected abstract void addReturnType(Template template, JSReferenceExpression referenceExpression, PsiFile psifile);

	protected abstract void addBody(Template template, JSReferenceExpression refExpr, PsiFile file);
}

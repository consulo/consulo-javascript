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

import com.intellij.codeInsight.template.Expression;
import com.intellij.codeInsight.template.Template;
import com.intellij.lang.javascript.psi.JSArgumentList;
import com.intellij.lang.javascript.psi.JSCallExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import consulo.annotations.RequiredReadAction;
import consulo.javascript.lang.JavaScriptFeature;

public class CreateJSFunctionOrMethodFix extends CreateJSFunctionFixBase
{
	protected final boolean myIsMethod;

	public CreateJSFunctionOrMethodFix(String name, boolean isMethod)
	{
		super(name, isMethod ? "javascript.create.method.intention.name" : "javascript.create.function.intention.name");
		myIsMethod = isMethod;
	}

	@Override
	protected void writeFunctionAndName(Template template, String createdMethodName, Set<JavaScriptFeature> features)
	{
		boolean ecma = features.contains(JavaScriptFeature.CLASS);
		if(!myIsMethod || ecma)
		{
			template.addTextSegment("function ");
		}
		template.addTextSegment(createdMethodName);
		if(myIsMethod && !ecma)
		{
			template.addTextSegment(" = function ");
		}
	}

	@Override
	protected void addParameters(Template template, JSReferenceExpression referenceExpression, PsiFile file, Set<JavaScriptFeature> features)
	{
		JSCallExpression methodInvokation = (JSCallExpression) referenceExpression.getParent();
		final JSArgumentList list = methodInvokation.getArgumentList();
		final JSExpression[] expressions = list.getArguments();
		int paramCount = expressions.length;

		for(int i = 0; i < paramCount; ++i)
		{
			if(i != 0)
			{
				template.addTextSegment(", ");
			}
			String var = null;

			final JSExpression passedParameterValue = expressions[i];
			if(passedParameterValue instanceof JSReferenceExpression)
			{
				var = ((JSReferenceExpression) passedParameterValue).getReferencedName();
			}

			if(var == null || var.length() == 0)
			{
				var = "param" + (i != 0 ? Integer.toString(i + 1) : "");
			}

			final String var1 = var;
			Expression expression = new MyExpression(var1);

			template.addVariable(var, expression, expression, true);
			if(features.contains(JavaScriptFeature.CLASS))
			{
				template.addTextSegment(":");
				guessExprTypeAndAddSuchVariable(passedParameterValue, template, var1, file, features);
			}
		}

	}

	@Override
	protected void addReturnType(Template template, JSReferenceExpression referenceExpression, PsiFile file)
	{
		guessTypeAndAddTemplateVariable(template, referenceExpression, file);
	}

	@Override
	protected void addBody(Template template, JSReferenceExpression refExpr, PsiFile file)
	{
		template.addEndVariable();
	}

	@RequiredReadAction
	@Override
	protected void buildTemplate(final Template template, JSReferenceExpression referenceExpression, Set<JavaScriptFeature> features, boolean staticContext,
			PsiFile file, PsiElement anchorParent)
	{
		super.buildTemplate(template, referenceExpression, features, staticContext, file, anchorParent);

		if(myIsMethod && !features.contains(JavaScriptFeature.CLASS))
		{
			addSemicolonSegment(template, file);
		}
	}
}

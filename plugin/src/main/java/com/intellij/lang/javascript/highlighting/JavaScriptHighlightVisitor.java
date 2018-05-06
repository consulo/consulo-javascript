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

package com.intellij.lang.javascript.highlighting;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.codeInsight.daemon.impl.HighlightInfoType;
import com.intellij.codeInsight.daemon.impl.HighlightVisitor;
import com.intellij.codeInsight.daemon.impl.analysis.HighlightInfoHolder;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptBundle;
import com.intellij.lang.javascript.psi.*;
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiNameIdentifierOwner;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.util.PsiUtilCore;
import consulo.annotations.RequiredReadAction;
import consulo.javascript.ide.hightlight.JavaScriptSyntaxHighlightKeys;
import consulo.javascript.lang.JavaScriptFeature;
import consulo.javascript.lang.JavaScriptTokenSets;
import consulo.javascript.lang.JavaScriptVersionUtil;
import consulo.javascript.psi.JSSimpleLiteralExpression;

public class JavaScriptHighlightVisitor extends JSElementVisitor implements HighlightVisitor
{
	private HighlightInfoHolder myHighlightInfoHolder;

	@Override
	public boolean suitableForFile(@Nonnull PsiFile psiFile)
	{
		return psiFile instanceof JSFile;
	}

	@Override
	@RequiredReadAction
	public void visitElement(PsiElement element)
	{
		super.visitElement(element);

		PsiElement parent = element.getParent();
		IElementType elementType = PsiUtilCore.getElementType(element);
		if((JavaScriptTokenSets.STRING_LITERALS.contains(elementType) || elementType == JSTokenTypes.IDENTIFIER) && parent instanceof JSProperty && ((JSProperty) parent).getNameIdentifier() ==
				element)
		{
			highlightPropertyName((JSProperty) parent, element);
		}
		else if(elementType == JSTokenTypes.IDENTIFIER)
		{
			addElementHighlight(parent, element);
		}
		else if(JSTokenTypes.CONTEXT_KEYWORDS.contains(elementType))
		{
			myHighlightInfoHolder.add(HighlightInfo.newHighlightInfo(HighlightInfoType.INFORMATION).textAttributes(JavaScriptSyntaxHighlightKeys.JS_KEYWORD).range(element).create());
		}
	}

	@Override
	@RequiredReadAction
	public void visitJSLiteralExpression(JSSimpleLiteralExpression node)
	{
		super.visitJSLiteralExpression(node);
		if(node.getLiteralElementType() == JSTokenTypes.NUMERIC_LITERAL)
		{
			String text = node.getText();
			if(StringUtil.startsWithIgnoreCase(text, "0o"))
			{
				reportFeatureUsage(node, JavaScriptFeature.OCTAL_LITERAL);
			}
			else if(StringUtil.startsWithIgnoreCase(text, "0b"))
			{
				reportFeatureUsage(node, JavaScriptFeature.BINARY_LITERAL);
			}
		}
	}

	@RequiredReadAction
	private void highlightPropertyName(@Nonnull JSProperty property, @Nonnull PsiElement nameIdentifier)
	{
		final JSExpression expression = property.getValue();
		TextAttributesKey type;

		if(expression instanceof JSFunctionExpression)
		{
			type = JavaScriptSyntaxHighlightKeys.JS_INSTANCE_MEMBER_FUNCTION;
		}
		else
		{
			type = JavaScriptSyntaxHighlightKeys.JS_INSTANCE_MEMBER_VARIABLE;
		}
		myHighlightInfoHolder.add(HighlightInfo.newHighlightInfo(HighlightInfoType.INFORMATION).range(nameIdentifier).textAttributes(type).create());
	}

	@Override
	@RequiredReadAction
	public void visitJSAttribute(JSAttribute jsAttribute)
	{
		super.visitJSAttribute(jsAttribute);

		myHighlightInfoHolder.add(HighlightInfo.newHighlightInfo(HighlightInfoType.INFORMATION).range(jsAttribute).textAttributes(JavaScriptSyntaxHighlightKeys.JS_METADATA).create());
	}

	@Override
	@RequiredReadAction
	public void visitJSParameter(JSParameter parameter)
	{
		super.visitJSParameter(parameter);

		JSExpression initializer = parameter.getInitializer();
		if(initializer != null)
		{
			reportFeatureUsage(initializer, JavaScriptFeature.PARAMETER_DEFAULT_VALUE);
		}

		PsiElement restElement = parameter.getRestElement();
		if(restElement != null)
		{
			reportFeatureUsage(restElement, JavaScriptFeature.REST_PARAMETER);
		}
	}

	@Override
	@RequiredReadAction
	public void visitJSReferenceExpression(JSReferenceExpression element)
	{
		super.visitJSReferenceExpression(element);

		PsiElement parent = element.getParent();

		if(parent instanceof PsiNameIdentifierOwner && ((PsiNameIdentifierOwner) parent).getNameIdentifier() == element)
		{
			return;
		}

		final ResolveResult[] results = element.multiResolve(false);

		PsiElement validResult = null;
		for(ResolveResult result : results)
		{
			if(result.isValidResult())
			{
				validResult = result.getElement();
				break;
			}
		}

		if(validResult == null)
		{
			return;
		}

		PsiElement referenceNameElement = element.getReferenceNameElement();
		if(referenceNameElement == null)
		{
			return;
		}
		addElementHighlight(validResult, referenceNameElement);
	}

	@Override
	@RequiredReadAction
	public void visitJSPrefixExpression(JSPrefixExpression expression)
	{
		super.visitJSPrefixExpression(expression);

		if(expression.getOperationSign() == JSTokenTypes.DOT_DOT_DOT)
		{
			PsiElement operatorElement = expression.getOperatorElement();
			assert operatorElement != null;
			reportFeatureUsage(operatorElement, JavaScriptFeature.SPREAD_OPERATOR);
		}
	}

	@RequiredReadAction
	private void addElementHighlight(@Nonnull PsiElement resolvedElement, @Nonnull PsiElement targetForHighlight)
	{
		boolean isStatic = false;
		boolean isMethod = false;
		boolean isFunction = false;
		boolean isField = false;
		TextAttributesKey type = null;

		if(resolvedElement instanceof JSAttributeListOwner)
		{
			if(resolvedElement instanceof JSClass)
			{
				type = DefaultLanguageHighlighterColors.CLASS_NAME;
			}
			else
			{
				final JSAttributeList attributeList = ((JSAttributeListOwner) resolvedElement).getAttributeList();

				if(attributeList != null)
				{
					isStatic = attributeList.hasModifier(JSAttributeList.ModifierType.STATIC);
				}

				isMethod = resolvedElement instanceof JSFunction;
				if(isMethod && !isClass(resolvedElement.getParent()))
				{
					isMethod = false;
					isFunction = true;
				}
			}
		}
		else if(resolvedElement instanceof JSDefinitionExpression)
		{
			final PsiElement parent = resolvedElement.getParent();
			if(parent instanceof JSAssignmentExpression)
			{
				final JSExpression jsExpression = ((JSAssignmentExpression) parent).getROperand();
				if(jsExpression instanceof JSFunctionExpression)
				{
					isMethod = true;
				}
				else
				{
					isField = true;
				}
			}
		}
		else if(resolvedElement instanceof JSProperty)
		{
			final JSExpression expression = ((JSProperty) resolvedElement).getValue();

			if(expression instanceof JSFunctionExpression)
			{
				isMethod = true;
			}
			else
			{
				isField = true;
			}
		}

		if(isMethod)
		{
			if(isStatic)
			{
				type = JavaScriptSyntaxHighlightKeys.JS_STATIC_MEMBER_FUNCTION;
			}
			else
			{
				type = JavaScriptSyntaxHighlightKeys.JS_INSTANCE_MEMBER_FUNCTION;
			}
		}
		else if(isFunction)
		{
			type = JavaScriptSyntaxHighlightKeys.JS_GLOBAL_FUNCTION;
		}
		else if(isField)
		{
			type = JavaScriptSyntaxHighlightKeys.JS_INSTANCE_MEMBER_VARIABLE;
		}

		if(type == null)
		{
			if(resolvedElement instanceof JSVariable)
			{
				myHighlightInfoHolder.add(buildHighlightForVariable(resolvedElement, targetForHighlight));
			}
			return;
		}

		myHighlightInfoHolder.add(HighlightInfo.newHighlightInfo(HighlightInfoType.INFORMATION).range(targetForHighlight).textAttributes(type).create());
	}

	@Nullable
	@RequiredReadAction
	private static HighlightInfo buildHighlightForVariable(@Nonnull final PsiElement element, @Nonnull final PsiElement markerAddTo)
	{
		TextAttributesKey type;

		if(element instanceof JSParameter)
		{
			type = JavaScriptSyntaxHighlightKeys.JS_PARAMETER;
		}
		else
		{
			if(isClass(element.getParent().getParent()))
			{
				final JSAttributeList attributeList = ((JSAttributeListOwner) element).getAttributeList();
				final boolean isStatic = attributeList != null && attributeList.hasModifier(JSAttributeList.ModifierType.STATIC);
				type = isStatic ? JavaScriptSyntaxHighlightKeys.JS_STATIC_MEMBER_VARIABLE : JavaScriptSyntaxHighlightKeys.JS_INSTANCE_MEMBER_VARIABLE;
			}
			else
			{
				if(PsiTreeUtil.getParentOfType(element, JSFunction.class) != null)
				{
					type = JavaScriptSyntaxHighlightKeys.JS_LOCAL_VARIABLE;
				}
				else
				{
					type = JavaScriptSyntaxHighlightKeys.JS_GLOBAL_VARIABLE;
				}
			}
		}

		if(type == null)
		{
			return null;
		}

		return HighlightInfo.newHighlightInfo(HighlightInfoType.INFORMATION).range(markerAddTo).textAttributes(type).create();
	}

	private void reportFeatureUsage(@Nonnull PsiElement element, @Nonnull JavaScriptFeature javaScriptFeature)
	{
		if(JavaScriptVersionUtil.containsFeature(element, javaScriptFeature))
		{
			return;
		}

		String message = JavaScriptBundle.message("this.feature.is.not.supported.by.current.language", javaScriptFeature.getName());

		myHighlightInfoHolder.add(HighlightInfo.newHighlightInfo(HighlightInfoType.ERROR).range(element).descriptionAndTooltip(message).create());
	}

	@Override
	public void visit(@Nonnull PsiElement element)
	{
		element.acceptChildren(this);
	}

	@Override
	public boolean analyze(@Nonnull PsiFile psiFile, boolean b, @Nonnull HighlightInfoHolder highlightInfoHolder, @Nonnull Runnable runnable)
	{
		myHighlightInfoHolder = highlightInfoHolder;
		runnable.run();
		return true;
	}

	@Nonnull
	@Override
	public HighlightVisitor clone()
	{
		return new JavaScriptHighlightVisitor();
	}

	@Override
	public int order()
	{
		return 0;
	}

	private static boolean isClass(final PsiElement element)
	{
		return element instanceof JSClass || element instanceof JSFile && element.getContext() != null;
	}
}

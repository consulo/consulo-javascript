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

package com.intellij.lang.javascript.impl.highlighting;

import consulo.annotation.component.ExtensionImpl;
import consulo.codeEditor.DefaultLanguageHighlighterColors;
import consulo.language.editor.rawHighlight.HighlightInfo;
import consulo.language.editor.rawHighlight.HighlightInfoType;
import consulo.language.editor.rawHighlight.HighlightVisitor;
import consulo.language.editor.rawHighlight.HighlightInfoHolder;
import com.intellij.lang.javascript.JSTokenTypes;
import consulo.javascript.language.JavaScriptBundle;
import com.intellij.lang.javascript.psi.*;
import consulo.colorScheme.TextAttributesKey;
import consulo.language.psi.*;
import consulo.language.ast.IElementType;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.ide.hightlight.JavaScriptSyntaxHighlightKeys;
import consulo.javascript.lang.JavaScriptContextKeywordElementType;
import consulo.javascript.language.JavaScriptFeature;
import consulo.javascript.lang.JavaScriptTokenSets;
import consulo.javascript.language.JavaScriptVersionUtil;
import consulo.javascript.psi.JSSimpleLiteralExpression;
import consulo.util.lang.StringUtil;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@ExtensionImpl
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
	public void visitJSBinaryExpression(JSBinaryExpression node)
	{
		super.visitJSBinaryExpression(node);

		IElementType operationSign = node.getOperationSign();
		if(operationSign == JSTokenTypes.MULTMULT)
		{
			reportFeatureUsage(node.getOperationElement(), JavaScriptFeature.EXPONENTIATION_OPERATOR);
		}
	}

	@Override
	@RequiredReadAction
	public void visitJSAssignmentExpression(JSAssignmentExpression node)
	{
		super.visitJSAssignmentExpression(node);

		IElementType operationSign = node.getOperationSign();
		if(operationSign == JSTokenTypes.MULT_MULT_EQ)
		{
			reportFeatureUsage(node.getOperationElement(), JavaScriptFeature.EXPONENTIATION_OPERATOR);
		}
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
		else if(JavaScriptContextKeywordElementType.containsKeyword(elementType))
		{
			myHighlightInfoHolder.add(HighlightInfo.newHighlightInfo(HighlightInfoType.INFORMATION).textAttributes(JavaScriptSyntaxHighlightKeys.JS_KEYWORD).range(element).create());
		}
	}

	@Override
	public void visitJSProperty(JSProperty node)
	{
		super.visitJSProperty(node);

		if(node instanceof JSFunction)
		{
			reportFeatureUsage(node.getNameIdentifier(), JavaScriptFeature.FUNCTION_PROPERTY);
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
		myHighlightInfoHolder.add(HighlightInfo.newHighlightInfo(HighlightInfoType.INFORMATION).needsUpdateOnTyping(false).range(nameIdentifier).textAttributes(type).create());
	}

	@Override
	@RequiredReadAction
	public void visitJSAttribute(JSAttribute jsAttribute)
	{
		super.visitJSAttribute(jsAttribute);

		myHighlightInfoHolder.add(HighlightInfo.newHighlightInfo(HighlightInfoType.INFORMATION).needsUpdateOnTyping(false).range(jsAttribute).textAttributes(JavaScriptSyntaxHighlightKeys
				.JS_METADATA).create());
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

		myHighlightInfoHolder.add(HighlightInfo.newHighlightInfo(HighlightInfoType.INFORMATION).needsUpdateOnTyping(false).range(targetForHighlight).textAttributes(type).create());
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

		return HighlightInfo.newHighlightInfo(HighlightInfoType.INFORMATION).range(markerAddTo).needsUpdateOnTyping(false).textAttributes(type).create();
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

	private static boolean isClass(final PsiElement element)
	{
		return element instanceof JSClass || element instanceof JSFile && element.getContext() != null;
	}
}

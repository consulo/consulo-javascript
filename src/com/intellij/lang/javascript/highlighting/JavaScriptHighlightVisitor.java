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

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.RequiredReadAction;
import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.codeInsight.daemon.impl.HighlightInfoType;
import com.intellij.codeInsight.daemon.impl.HighlightVisitor;
import com.intellij.codeInsight.daemon.impl.analysis.HighlightInfoHolder;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.index.JSNamedElementProxy;
import com.intellij.lang.javascript.index.JSNamespace;
import com.intellij.lang.javascript.psi.*;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.util.PsiUtilCore;

public class JavaScriptHighlightVisitor extends JSElementVisitor implements HighlightVisitor
{
	@NonNls
	private static final String PARAMETER_MESSAGE = "parameter";

	private HighlightInfoHolder myHighlightInfoHolder;

	@Override
	public boolean suitableForFile(@NotNull PsiFile psiFile)
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
		if((elementType == JSTokenTypes.STRING_LITERAL || elementType == JSTokenTypes.IDENTIFIER) && parent instanceof JSProperty && ((JSProperty) parent).getNameIdentifier() == element)
		{
			highlightPropertyName((JSProperty) parent, element);
		}
	}

	@RequiredReadAction
	private void highlightPropertyName(@NotNull JSProperty property, @NotNull PsiElement nameIdentifier)
	{
		final JSExpression expression = property.getValue();
		TextAttributesKey type = null;

		if(expression instanceof JSFunctionExpression)
		{
			type = JSHighlighter.JS_INSTANCE_MEMBER_FUNCTION;
		}
		else
		{
			type = JSHighlighter.JS_INSTANCE_MEMBER_VARIABLE;
		}
		myHighlightInfoHolder.add(HighlightInfo.newHighlightInfo(HighlightInfoType.INFORMATION).range(nameIdentifier).textAttributes(type).create());
	}

	@Override
	@RequiredReadAction
	public void visitJSAttribute(JSAttribute jsAttribute)
	{
		super.visitJSAttribute(jsAttribute);

		myHighlightInfoHolder.add(createHighlightInfo(jsAttribute, JSHighlighter.JS_METADATA, null));
	}

	@Override
	@RequiredReadAction
	public void visitJSReferenceExpression(JSReferenceExpression element)
	{
		super.visitJSReferenceExpression(element);

		final ResolveResult[] results = element.multiResolve(false);
		boolean isStatic = false;
		boolean isMethod = false;
		boolean isFunction = false;
		boolean isVariable = false;
		boolean isField = false;
		TextAttributesKey type = null;
		@NonNls String text = null;

		for(ResolveResult r : results)
		{
			final PsiElement resolve = r.getElement();

			if(resolve instanceof JSNamedElementProxy)
			{
				final JSNamedElementProxy elementProxy = (JSNamedElementProxy) resolve;
				final JSNamedElementProxy.NamedItemType namedItemType = elementProxy.getType();

				if(namedItemType == JSNamedElementProxy.NamedItemType.AttributeValue)
				{
					type = JSHighlighter.JS_INSTANCE_MEMBER_VARIABLE;
					text = "field";
				}
				else
				{
					isStatic |= elementProxy.hasProperty(JSNamedElementProxy.Property.Static);

					isMethod |= (namedItemType == JSNamedElementProxy.NamedItemType.MemberFunction || namedItemType == JSNamedElementProxy.NamedItemType.FunctionProperty);
					isFunction |= namedItemType == JSNamedElementProxy.NamedItemType.Function;
					isVariable |= namedItemType == JSNamedElementProxy.NamedItemType.Variable;
					isField |= namedItemType == JSNamedElementProxy.NamedItemType.Definition || namedItemType == JSNamedElementProxy.NamedItemType.Property;

					if(namedItemType == JSNamedElementProxy.NamedItemType.FunctionExpression)
					{
						final JSNamespace namespace = elementProxy.getNamespace();

						if(namespace.getNameId() == -1)
						{
							isFunction = true;
						}
						else
						{
							isMethod = true;
						}
					}
				}
			}
			else if(resolve instanceof JSAttributeListOwner)
			{
				if(resolve instanceof JSVariable)
				{
					myHighlightInfoHolder.add(buildHighlightForVariable(resolve, element));
				}

				final JSAttributeList attributeList = ((JSAttributeListOwner) resolve).getAttributeList();

				if(attributeList != null)
				{
					isStatic |= attributeList.hasModifier(JSAttributeList.ModifierType.STATIC);
				}

				isMethod = resolve instanceof JSFunction;
				if(isMethod && !isClass(resolve.getParent()))
				{
					isMethod = false;
					isFunction = true;
				}
			}
			else if(resolve instanceof JSDefinitionExpression)
			{
				final PsiElement parent = resolve.getParent();
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
			else if(resolve instanceof JSProperty)
			{
				final JSExpression expression = ((JSProperty) resolve).getValue();

				if(expression instanceof JSFunctionExpression)
				{
					isMethod = true;
				}
				else
				{
					isField = true;
				}
			}
		}

		if(isMethod)
		{
			if(isStatic)
			{
				type = JSHighlighter.JS_STATIC_MEMBER_FUNCTION;
			}
			else
			{
				type = JSHighlighter.JS_INSTANCE_MEMBER_FUNCTION;
			}
		}
		else if(isFunction)
		{
			type = JSHighlighter.JS_GLOBAL_FUNCTION;
		}
		else if(isVariable)
		{
			type = JSHighlighter.JS_GLOBAL_VARIABLE;
		}
		else if(isField)
		{
			type = JSHighlighter.JS_INSTANCE_MEMBER_VARIABLE;
		}

		myHighlightInfoHolder.add(createHighlightInfo(element, type, text));
	}

	@Nullable
	@RequiredReadAction
	private static HighlightInfo buildHighlightForVariable(@NotNull final PsiElement element, @NotNull final PsiElement markerAddTo)
	{
		TextAttributesKey type;
		@NonNls String text;

		if(element instanceof JSParameter)
		{
			type = JSHighlighter.JS_PARAMETER;
			text = PARAMETER_MESSAGE;
		}
		else
		{
			if(isClass(element.getParent().getParent()))
			{
				final JSAttributeList attributeList = ((JSAttributeListOwner) element).getAttributeList();
				final boolean isStatic = attributeList != null && attributeList.hasModifier(JSAttributeList.ModifierType.STATIC);
				type = isStatic ? JSHighlighter.JS_STATIC_MEMBER_VARIABLE : JSHighlighter.JS_INSTANCE_MEMBER_VARIABLE;
				text = (isStatic ? "static " : "") + "field";
			}
			else
			{
				if(PsiTreeUtil.getParentOfType(element, JSFunction.class) != null)
				{
					type = JSHighlighter.JS_LOCAL_VARIABLE;
					text = "local variable";
				}
				else
				{
					type = JSHighlighter.JS_GLOBAL_VARIABLE;
					text = "global variable";
				}
			}
		}

		return createHighlightInfo(markerAddTo, type, text);
	}

	@Override
	public void visit(@NotNull PsiElement element)
	{
		element.acceptChildren(this);
	}

	@Override
	public boolean analyze(@NotNull PsiFile psiFile, boolean b, @NotNull HighlightInfoHolder highlightInfoHolder, @NotNull Runnable runnable)
	{
		myHighlightInfoHolder = highlightInfoHolder;
		runnable.run();
		return true;
	}

	@NotNull
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
		if(element instanceof JSClass)
		{
			return true;
		}
		if(element instanceof JSFile && element.getContext() != null)
		{
			return true;
		}
		return false;
	}

	@Nullable
	@RequiredReadAction
	private static HighlightInfo createHighlightInfo(@NotNull final PsiElement element, @Nullable final TextAttributesKey type, @Nullable @NonNls final String text)
	{
		if(type == null)
		{
			return null;
		}
		PsiElement markedNode = element.getLastChild();
		if(element instanceof JSNamedElement)
		{
			ASTNode nameNode = ((JSNamedElement) element).findNameIdentifier();
			if(nameNode != null)
			{
				markedNode = nameNode.getPsi();
			}
		}
		else if(element instanceof JSAttribute)
		{
			markedNode = element;
		}

		if(markedNode == null)
		{
			return null;
		}

		HighlightInfo.Builder builder = HighlightInfo.newHighlightInfo(HighlightInfoType.INFORMATION).range(markedNode).textAttributes(type);

		if(text != null)
		{
			builder = builder.descriptionAndTooltip(text);
		}

		return builder.create();
	}
}

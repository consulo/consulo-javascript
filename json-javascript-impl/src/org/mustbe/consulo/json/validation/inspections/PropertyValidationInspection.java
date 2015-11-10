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

package org.mustbe.consulo.json.validation.inspections;

import java.util.ArrayDeque;
import java.util.Deque;

import org.jetbrains.annotations.NotNull;
import org.mustbe.consulo.RequiredReadAction;
import org.mustbe.consulo.json.validation.JsonFileDescriptorProviders;
import org.mustbe.consulo.json.validation.descriptor.JsonObjectDescriptor;
import org.mustbe.consulo.json.validation.descriptor.JsonPropertyDescriptor;
import org.mustbe.consulo.json.validation.descriptor.JsonPropertyType;
import com.intellij.codeInspection.LocalInspectionTool;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import com.intellij.lang.javascript.psi.JSObjectLiteralExpression;
import com.intellij.lang.javascript.psi.JSProperty;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.util.PsiUtilCore;
import com.intellij.util.PairProcessor;

/**
 * @author VISTALL
 * @since 10.11.2015
 */
public class PropertyValidationInspection extends LocalInspectionTool
{
	@NotNull
	@Override
	public PsiElementVisitor buildVisitor(@NotNull final ProblemsHolder holder, boolean isOnTheFly)
	{
		return new JSElementVisitor()
		{
			@Override
			@RequiredReadAction
			public void visitJSLiteralExpression(JSLiteralExpression node)
			{
				PsiElement firstChild = node.getFirstChild();
				IElementType elementType = PsiUtilCore.getElementType(firstChild);
				if(elementType == null)
				{
					return;
				}

				JsonPropertyType propertyType = null;
				if(elementType == JSTokenTypes.NUMERIC_LITERAL)
				{
					propertyType = JsonPropertyType.Number;
				}
				else if(elementType == JSTokenTypes.STRING_LITERAL)
				{
					propertyType = JsonPropertyType.String;
				}
				else if(elementType == JSTokenTypes.NULL_KEYWORD)
				{
					propertyType = JsonPropertyType.Null;
				}
				else if(elementType == JSTokenTypes.TRUE_KEYWORD || elementType == JSTokenTypes.FALSE_KEYWORD)
				{
					propertyType = JsonPropertyType.Boolean;
				}

				if(propertyType == null)
				{
					return;
				}
				validateValue(node, propertyType, holder);
			}

			@Override
			@RequiredReadAction
			public void visitJSObjectLiteralExpression(JSObjectLiteralExpression node)
			{
				validateValue(node, JsonPropertyType.Object, holder);
			}

			@Override
			@RequiredReadAction
			public void visitJSProperty(JSProperty node)
			{
				JsonObjectDescriptor rootDescriptor = JsonFileDescriptorProviders.getRootDescriptor(node.getContainingFile());
				if(rootDescriptor == null)
				{
					return;
				}

				final Deque<JSProperty> queue = new ArrayDeque<JSProperty>();
				PsiTreeUtil.treeWalkUp(node, null, new PairProcessor<PsiElement, PsiElement>()
				{
					@Override
					public boolean process(PsiElement element, PsiElement element2)
					{
						if(element instanceof JSProperty)
						{
							queue.addFirst((JSProperty) element);
						}
						return true;
					}
				});

				JsonObjectDescriptor currentObject = rootDescriptor;
				for(JSProperty property : queue)
				{
					String name = property.getName();
					if(name == null)
					{
						return;
					}

					JsonPropertyDescriptor propertyDescriptor = currentObject.getProperty(name);
					if(propertyDescriptor == null)
					{
						if(node == property)
						{
							PsiElement nameIdentifier = node.getNameIdentifier();
							assert nameIdentifier != null;

							holder.registerProblem(nameIdentifier, "Undefined property", ProblemHighlightType.ERROR);
						}
						return;
					}
					else if(propertyDescriptor.getValue() instanceof JsonObjectDescriptor)
					{
						currentObject = (JsonObjectDescriptor) propertyDescriptor.getValue();
					}
					else
					{
						return;
					}
				}
			}
		};
	}

	@RequiredReadAction
	private static void validateValue(PsiElement value, JsonPropertyType actualType, ProblemsHolder holder)
	{
		PsiElement parent = value.getParent();
		if(!(parent instanceof JSProperty))
		{
			return;
		}

		JsonObjectDescriptor rootDescriptor = JsonFileDescriptorProviders.getRootDescriptor(value.getContainingFile());
		if(rootDescriptor == null)
		{
			return;
		}

		final Deque<JSProperty> queue = new ArrayDeque<JSProperty>();
		PsiTreeUtil.treeWalkUp(value, null, new PairProcessor<PsiElement, PsiElement>()
		{
			@Override
			public boolean process(PsiElement element, PsiElement element2)
			{
				if(element instanceof JSProperty)
				{
					queue.addFirst((JSProperty) element);
				}
				return true;
			}
		});

		JsonPropertyDescriptor currentProperty = null;
		JsonObjectDescriptor currentObject = rootDescriptor;
		for(JSProperty property : queue)
		{
			String name = property.getName();
			if(name == null)
			{
				return;
			}

			currentProperty = currentObject.getProperty(name);
			if(currentProperty == null)
			{
				return;
			}
			else if(currentProperty.getValue() instanceof JsonObjectDescriptor)
			{
				currentObject = (JsonObjectDescriptor) currentProperty.getValue();
			}
			else
			{
				break;
			}
		}

		if(currentProperty == null)
		{
			return;
		}

		JsonPropertyType type = currentProperty.getType();
		if(type != actualType)
		{
			holder.registerProblem(value, "Wrong property value. Expected: " + type + ", actual: " + actualType, ProblemHighlightType.GENERIC_ERROR);
		}
	}
}

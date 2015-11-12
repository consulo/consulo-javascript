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

import gnu.trove.THashSet;

import java.lang.reflect.Array;
import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.Set;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.RequiredReadAction;
import org.mustbe.consulo.json.jom.Null;
import org.mustbe.consulo.json.validation.JsonFileDescriptorProviders;
import org.mustbe.consulo.json.validation.descriptor.JsonObjectDescriptor;
import org.mustbe.consulo.json.validation.descriptor.JsonPropertyDescriptor;
import com.intellij.codeInspection.LocalInspectionTool;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSArrayLiteralExpression;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import com.intellij.lang.javascript.psi.JSObjectLiteralExpression;
import com.intellij.lang.javascript.psi.JSProperty;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.util.PsiUtilCore;
import com.intellij.util.PairProcessor;
import com.intellij.util.containers.ContainerUtil;

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
				validateValue(node, holder);
			}

			@Override
			@RequiredReadAction
			public void visitJSObjectLiteralExpression(JSObjectLiteralExpression node)
			{
				validateValue(node, holder);
			}

			@Override
			@RequiredReadAction
			public void visitJSArrayLiteralExpression(JSArrayLiteralExpression node)
			{
				validateValue(node, holder);
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

				Collection<JSProperty> jsProperties = buildPropertiesAsTree(node, rootDescriptor);
				if(jsProperties.isEmpty())
				{
					return;
				}

				JsonObjectDescriptor currentObject = rootDescriptor;
				for(JSProperty property : jsProperties)
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
	public static Collection<JSProperty> buildPropertiesAsTree(PsiElement element, @Nullable JsonObjectDescriptor objectDescriptor)
	{
		JsonObjectDescriptor rootDescriptor = objectDescriptor == null ? JsonFileDescriptorProviders.getRootDescriptor(element.getContainingFile()) : objectDescriptor;
		if(rootDescriptor == null)
		{
			return Collections.emptyList();
		}

		final Deque<JSProperty> queue = new ArrayDeque<JSProperty>();
		PsiTreeUtil.treeWalkUp(element, null, new PairProcessor<PsiElement, PsiElement>()
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
		return queue;
	}

	@Nullable
	@RequiredReadAction
	private static Class<?> getTypeOfExpression(@NotNull PsiElement node)
	{
		if(node instanceof JSLiteralExpression)
		{
			PsiElement firstChild = node.getFirstChild();
			IElementType elementType = PsiUtilCore.getElementType(firstChild);
			if(elementType == null)
			{
				return null;
			}

			Class<?> propertyType = null;
			if(elementType == JSTokenTypes.NUMERIC_LITERAL)
			{
				propertyType = Number.class;
			}
			else if(elementType == JSTokenTypes.STRING_LITERAL)
			{
				propertyType = String.class;
			}
			else if(elementType == JSTokenTypes.NULL_KEYWORD)
			{
				propertyType = Null.class;
			}
			else if(elementType == JSTokenTypes.TRUE_KEYWORD || elementType == JSTokenTypes.FALSE_KEYWORD)
			{
				propertyType = Boolean.class;
			}

			if(propertyType == null)
			{
				return null;
			}
			return propertyType;
		}
		else if(node instanceof JSObjectLiteralExpression)
		{
			return Object.class;
		}
		else if(node instanceof JSArrayLiteralExpression)
		{
			Set<Class<?>> types = new THashSet<Class<?>>();
			JSExpression[] expressions = ((JSArrayLiteralExpression) node).getExpressions();
			for(JSExpression expression : expressions)
			{
				Class<?> typeOfExpression = getTypeOfExpression(expression);
				ContainerUtil.addIfNotNull(types, typeOfExpression);
			}

			int size = types.size();
			switch(size)
			{
				case 0:
					return null;
				case 1:
					Class<?> firstItem = ContainerUtil.getFirstItem(types);
					assert firstItem != null;
					// calc
					Object arrayOfZero = Array.newInstance(firstItem, 0);
					return arrayOfZero.getClass();
				default:
					return Object[].class;
			}
		}
		return null;
	}

	@Nullable
	@RequiredReadAction
	public static JsonPropertyDescriptor findPropertyDescriptor(@NotNull JSProperty jsProperty)
	{
		JsonObjectDescriptor rootDescriptor = JsonFileDescriptorProviders.getRootDescriptor(jsProperty.getContainingFile());
		if(rootDescriptor == null)
		{
			return null;
		}

		Collection<JSProperty> jsProperties = buildPropertiesAsTree(jsProperty, rootDescriptor);
		if(jsProperties.isEmpty())
		{
			return null;
		}

		JsonPropertyDescriptor currentProperty = null;
		JsonObjectDescriptor currentObject = rootDescriptor;
		for(JSProperty property : jsProperties)
		{
			String name = property.getName();
			if(name == null)
			{
				return null;
			}

			currentProperty = currentObject.getProperty(name);
			if(currentProperty == null)
			{
				return null;
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

		return currentProperty;
	}

	@RequiredReadAction
	private static void validateValue(@NotNull PsiElement value, @NotNull ProblemsHolder holder)
	{
		Class<?> actualType = getTypeOfExpression(value);
		if(actualType == null)
		{
			return;
		}
		PsiElement parent = value.getParent();
		if(!(parent instanceof JSProperty))
		{
			return;
		}

		JsonPropertyDescriptor currentProperty = findPropertyDescriptor((JSProperty) parent);
		if(currentProperty == null)
		{
			return;
		}

		Class type = currentProperty.getType();
		if(type != actualType)
		{
			holder.registerProblem(value, "Wrong property value. Expected: " + StringUtil.decapitalize(type.getSimpleName()) + ", actual: " + StringUtil.decapitalize(actualType.getSimpleName()),
					ProblemHighlightType.GENERIC_ERROR);
		}
	}
}

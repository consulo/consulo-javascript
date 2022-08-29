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

package consulo.json.validation.inspections;

import consulo.application.util.CachedValueProvider;
import consulo.application.util.CachedValuesManager;
import consulo.language.ast.IElementType;
import consulo.language.editor.inspection.LocalInspectionTool;
import consulo.language.editor.inspection.ProblemHighlightType;
import consulo.language.editor.inspection.ProblemsHolder;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import consulo.language.editor.rawHighlight.HighlightDisplayLevel;
import consulo.language.psi.PsiModificationTracker;
import consulo.language.psi.PsiUtilCore;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.util.lang.StringUtil;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.util.collection.ContainerUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.lang.JavaScriptTokenSets;
import consulo.javascript.psi.JSSimpleLiteralExpression;
import consulo.json.validation.JsonFileDescriptorProviders;
import consulo.json.validation.NativeArray;
import consulo.json.validation.descriptor.JsonObjectDescriptor;
import consulo.json.validation.descriptor.JsonPropertyDescriptor;
import consulo.util.lang.function.PairProcessor;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.*;

/**
 * @author VISTALL
 * @since 10.11.2015
 */
public class PropertyValidationInspection extends LocalInspectionTool
{
	@Nonnull
	@Override
	public PsiElementVisitor buildVisitor(@Nonnull final ProblemsHolder holder, boolean isOnTheFly)
	{
		return new JSElementVisitor()
		{
			@Override
			@RequiredReadAction
			public void visitJSLiteralExpression(JSSimpleLiteralExpression node)
			{
				validateValue(node, holder);
			}

			@Override
			@RequiredReadAction
			public void visitJSPrefixExpression(JSPrefixExpression expression)
			{
				validateValue(expression, holder);
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

		final Deque<JSProperty> queue = new ArrayDeque<>();
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
	private static Object getTypeOfExpression(@Nonnull PsiElement node)
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
			else if(JavaScriptTokenSets.STRING_LITERALS.contains(elementType))
			{
				propertyType = String.class;
			}
			else if(elementType == JSTokenTypes.NULL_KEYWORD)
			{
				propertyType = Void.class;
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
		else if(node instanceof JSPrefixExpression)
		{
			return Number.class;
		}
		else if(node instanceof JSObjectLiteralExpression)
		{
			return Object.class;
		}
		else if(node instanceof JSArrayLiteralExpression)
		{
			Set<Object> types = new HashSet<>();
			JSExpression[] expressions = ((JSArrayLiteralExpression) node).getExpressions();
			for(JSExpression expression : expressions)
			{
				if(expression == null)
				{
					continue;
				}

				Object typeOfExpression = getTypeOfExpression(expression);
				ContainerUtil.addIfNotNull(types, typeOfExpression);
			}

			int size = types.size();
			switch(size)
			{
				case 0:
					return null;
				case 1:
					Object firstItem = ContainerUtil.getFirstItem(types);
					assert firstItem != null;
					return new NativeArray(firstItem);
				default:
					return new NativeArray(Object.class);
			}
		}
		return null;
	}

	@Nullable
	@RequiredReadAction
	public static JsonPropertyDescriptor findPropertyDescriptor(@Nonnull final JSProperty jsProperty)
	{
		return CachedValuesManager.getManager(jsProperty.getProject()).createCachedValue(new CachedValueProvider<JsonPropertyDescriptor>()
		{
			@Nullable
			@Override
			@RequiredReadAction
			public Result<JsonPropertyDescriptor> compute()
			{
				return Result.create(findPropertyDescriptorImpl(jsProperty), jsProperty, PsiModificationTracker.OUT_OF_CODE_BLOCK_MODIFICATION_COUNT);
			}
		}, false).getValue();
	}

	@Nullable
	@RequiredReadAction
	private static JsonPropertyDescriptor findPropertyDescriptorImpl(@Nonnull JSProperty jsProperty)
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

			Object value = currentProperty.getValue();
			if(value instanceof JsonObjectDescriptor)
			{
				currentObject = (JsonObjectDescriptor) value;
			}
			else if(value instanceof NativeArray)
			{
				Object componentType = ((NativeArray) value).getComponentType();
				if(componentType instanceof JsonObjectDescriptor)
				{
					currentObject = (JsonObjectDescriptor) componentType;
				}
			}
			else
			{
				break;
			}
		}

		return currentProperty;
	}

	@RequiredReadAction
	private static void validateValue(@Nonnull PsiElement value, @Nonnull ProblemsHolder holder)
	{
		Object actualType = getTypeOfExpression(value);
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

		Object expectedValue = currentProperty.getValue();
		if(!isInheritable(currentProperty, expectedValue, actualType))
		{
			holder.registerProblem(value, "Wrong property value. Expected: " + getSimpleName(expectedValue) + ", actual: " + getSimpleName(actualType), ProblemHighlightType.GENERIC_ERROR);
		}

		if(currentProperty.isDeprecated())
		{
			PsiElement nameIdentifier = ((JSProperty) parent).getNameIdentifier();
			assert nameIdentifier != null;
			holder.registerProblem(nameIdentifier, "Deprecated property", ProblemHighlightType.LIKE_DEPRECATED);
		}
	}

	public static boolean isInheritable(JsonPropertyDescriptor currentProperty, Object expected, Object actual)
	{
		// null value
		if(currentProperty.isNullable() && actual == Void.class)
		{
			return true;
		}

		if(expected instanceof Class && actual instanceof Class)
		{
			return expected == actual;
		}

		if(expected instanceof JsonObjectDescriptor && actual == Object.class)
		{
			return true;
		}

		if(expected instanceof NativeArray && actual instanceof NativeArray)
		{
			return isInheritable(currentProperty, ((NativeArray) expected).getComponentType(), ((NativeArray) actual).getComponentType());
		}
		return false;
	}

	@Nonnull
	private static String getSimpleName(Object o)
	{
		if(o instanceof Class)
		{
			if(o == Void.class)
			{
				return "null";
			}
			return StringUtil.decapitalize(((Class) o).getSimpleName());
		}
		else if(o instanceof JsonObjectDescriptor)
		{
			return getSimpleName(Object.class);
		}
		else if(o instanceof NativeArray)
		{
			return getSimpleName(((NativeArray) o).getComponentType()) + "[]";
		}
		return "null";
	}

	@Nonnull
	@Override
	public String getGroupDisplayName()
	{
		return "JSON";
	}

	@Nonnull
	@Override
	public String getDisplayName()
	{
		return "Property validation";
	}

	@Nonnull
	@Override
	public HighlightDisplayLevel getDefaultLevel()
	{
		return HighlightDisplayLevel.ERROR;
	}
}

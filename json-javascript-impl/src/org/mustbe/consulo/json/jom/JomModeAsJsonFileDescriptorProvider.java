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

package org.mustbe.consulo.json.jom;

import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.math.BigInteger;
import java.util.HashSet;
import java.util.Set;

import org.jetbrains.annotations.NotNull;
import org.mustbe.consulo.RequiredReadAction;
import org.mustbe.consulo.json.validation.JsonFileDescriptorProvider;
import org.mustbe.consulo.json.validation.descriptor.JsonObjectDescriptor;
import org.mustbe.consulo.json.validation.descriptor.JsonSimplePropertyType;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiFile;
import com.intellij.util.ObjectUtil;
import com.intellij.util.containers.ContainerUtil;

/**
 * @author VISTALL
 * @since 10.11.2015
 */
public class JomModeAsJsonFileDescriptorProvider implements JsonFileDescriptorProvider
{
	@RequiredReadAction
	@Override
	public boolean isMyFile(@NotNull PsiFile file)
	{
		return JomManager.getInstance(file.getProject()).getFileElement(file) != null;
	}

	@RequiredReadAction
	@Override
	public void fillRootObject(@NotNull JsonObjectDescriptor root, @NotNull PsiFile file)
	{
		JomFileElement<JomElement> fileElement = JomManager.getInstance(file.getProject()).getFileElement(file);
		if(fileElement == null)
		{
			return;
		}

		fillDescriptor(root, fileElement.getFileDescriptor().getDefinitionClass());
	}

	private static void fillDescriptor(JsonObjectDescriptor objectDescriptor, Class<?> clazz)
	{
		Set<Method> methods = new HashSet<Method>();
		collectMethods(clazz, methods, new HashSet<Class<?>>());

		for(Method method : methods)
		{
			JomProperty jomProperty = method.getAnnotation(JomProperty.class);
			if(jomProperty == null)
			{
				continue;
			}

			String propertyName = StringUtil.getPropertyName(method.getName());
			propertyName = ObjectUtil.notNull(propertyName, method.getName());
			if(!StringUtil.isEmpty(jomProperty.value()))
			{
				propertyName = jomProperty.value();
			}

			Class<?> returnType = method.getReturnType();
			if(returnType == JomPropertyValue.class)
			{
				Type genericReturnType = method.getGenericReturnType();
				if(!(genericReturnType instanceof ParameterizedType))
				{
					throw new IllegalArgumentException("No generic arguments for method: " + method.getName() + ", class: " + method.getDeclaringClass().getName());
				}

				Type[] actualTypeArguments = ((ParameterizedType) genericReturnType).getActualTypeArguments();

				Class<?> actualTypeArgument = (Class<?>) actualTypeArguments[0];
				if(actualTypeArgument == String.class)
				{
					objectDescriptor.addSimpleProperty(propertyName, JsonSimplePropertyType.String);
				}
				else if(actualTypeArgument == Boolean.class)
				{
					objectDescriptor.addSimpleProperty(propertyName, JsonSimplePropertyType.Boolean);
				}
				else if(actualTypeArgument == Void.class)
				{
					objectDescriptor.addSimpleProperty(propertyName, JsonSimplePropertyType.Null);
				}
				else if(actualTypeArgument == Byte.class ||
						actualTypeArgument == Short.class ||
						actualTypeArgument == Integer.class ||
						actualTypeArgument == Long.class ||
						actualTypeArgument == Float.class ||
						actualTypeArgument == Double.class ||
						actualTypeArgument == BigInteger.class)
				{
					objectDescriptor.addSimpleProperty(propertyName, JsonSimplePropertyType.Number);
				}
			}
			else
			{
				JsonObjectDescriptor another = new JsonObjectDescriptor();
				fillDescriptor(another, returnType);
				objectDescriptor.addObjectProperty(propertyName, another);
			}
		}
	}

	private static void collectMethods(Class<?> clazz, Set<Method> methods, Set<Class<?>> processClasses)
	{
		if(processClasses.contains(clazz))
		{
			return;
		}

		processClasses.add(clazz);

		Method[] declaredMethods = clazz.getDeclaredMethods();
		ContainerUtil.addAllNotNull(methods, declaredMethods);

		Class<?>[] interfaces = clazz.getInterfaces();
		for(Class<?> anInterface : interfaces)
		{
			collectMethods(anInterface, methods, processClasses);
		}
	}
}

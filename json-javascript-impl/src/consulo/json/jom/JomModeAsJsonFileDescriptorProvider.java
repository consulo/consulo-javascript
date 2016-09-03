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

package consulo.json.jom;

import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.math.BigInteger;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import consulo.annotations.RequiredReadAction;
import consulo.json.validation.JsonFileDescriptorProvider;
import consulo.json.validation.NativeArray;
import consulo.json.validation.descriptor.JsonObjectDescriptor;
import consulo.json.validation.descriptor.JsonPropertyDescriptor;
import com.intellij.psi.PsiFile;
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
			String jsonGetPropertyName = JomUtil.getJsonGetPropertyName(method);
			if(jsonGetPropertyName == null)
			{
				continue;
			}

			JsonPropertyDescriptor propertyDescriptor = fillObjectDescriptor(objectDescriptor, method.getReturnType(), method.getGenericReturnType(), jsonGetPropertyName);

			if(method.isAnnotationPresent(Deprecated.class))
			{
				propertyDescriptor.deprecated();
			}
		}
	}

	@NotNull
	private static JsonPropertyDescriptor fillObjectDescriptor(JsonObjectDescriptor objectDescriptor, @NotNull Class<?> classType, @NotNull Type genericType, @Nullable String propertyName)
	{
		if(classType.isArray())
		{
			Class<?> componentType = classType.getComponentType();

			return objectDescriptor.addProperty(propertyName, new NativeArray(componentType));
		}
		else if(classType == Collection.class || classType == Set.class || classType == List.class)
		{
			if(!(genericType instanceof ParameterizedType))
			{
				throw new IllegalArgumentException();
			}

			Type[] actualTypeArguments = ((ParameterizedType) genericType).getActualTypeArguments();

			return objectDescriptor.addProperty(propertyName, new NativeArray(actualTypeArguments[0]));
		}
		else if(classType == boolean.class)
		{
			return objectDescriptor.addProperty(propertyName, Boolean.class);
		}
		else if(classType == Boolean.class)
		{
			return objectDescriptor.addProperty(propertyName, Boolean.class).notNull();
		}
		else if(classType == String.class)
		{
			return objectDescriptor.addProperty(propertyName, String.class);
		}
		else if(classType == byte.class ||
				classType == short.class ||
				classType == int.class ||
				classType == long.class ||
				classType == float.class ||
				classType == double.class)
		{
			return objectDescriptor.addProperty(propertyName, Number.class).notNull();
		}
		else if(classType == Byte.class ||
				classType == Short.class ||
				classType == Integer.class ||
				classType == Long.class ||
				classType == Float.class ||
				classType == Double.class ||
				classType == BigInteger.class)
		{
			return objectDescriptor.addProperty(propertyName, Number.class);
		}
		else if(classType == Map.class)
		{
			if(!(genericType instanceof ParameterizedType))
			{
				throw new IllegalArgumentException();
			}

			Type[] actualTypeArguments = ((ParameterizedType) genericType).getActualTypeArguments();

			Class rawType;
			Type actualTypeArgument = actualTypeArguments[1];
			if(actualTypeArgument instanceof ParameterizedType)
			{
				rawType = (Class) ((ParameterizedType) actualTypeArgument).getRawType();
			}
			else
			{
				rawType = (Class) actualTypeArgument;
			}

			JsonObjectDescriptor child = new JsonObjectDescriptor();
			fillObjectDescriptor(child, rawType, actualTypeArguments[1], null);

			return objectDescriptor.addProperty(propertyName, child);
		}
		else
		{
			JsonObjectDescriptor another = new JsonObjectDescriptor();
			fillDescriptor(another, classType);
			return objectDescriptor.addProperty(propertyName, another);
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

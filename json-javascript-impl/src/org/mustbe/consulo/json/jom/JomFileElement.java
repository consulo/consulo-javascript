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

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.openapi.util.NotNullLazyValue;

/**
 * @author VISTALL
 * @since 10.11.2015
 */
public class JomFileElement<T extends JomElement>
{
	private final JomFileDescriptor<T> myFileDescriptor;
	private final JSFile myPsiFile;
	private final NotNullLazyValue<T> myRootValue = new NotNullLazyValue<T>()
	{
		@NotNull
		@Override
		protected T compute()
		{
			//noinspection unchecked
			return (T) createProxy(JomFileElement.class.getClassLoader(), myFileDescriptor.getDefinitionClass());
		}
	};

	public JomFileElement(JSFile psiFile, JomFileDescriptor<T> fileDescriptor)
	{
		myPsiFile = psiFile;
		myFileDescriptor = fileDescriptor;
	}

	@NotNull
	public T getRootElement()
	{
		return myRootValue.getValue();
	}

	@NotNull
	public JomFileDescriptor<T> getFileDescriptor()
	{
		return myFileDescriptor;
	}

	@NotNull
	public JSFile getFile()
	{
		return myPsiFile;
	}

	@NotNull
	public static JomElement createProxy(@NotNull ClassLoader classLoader, @NotNull Class<?> interfaceClass)
	{
		return (JomElement) Proxy.newProxyInstance(classLoader, new Class[]{interfaceClass}, new InvocationHandler()
		{
			@Override
			public Object invoke(Object proxy, Method method, Object[] args) throws Throwable
			{
				return null;
			}
		});
	}
}

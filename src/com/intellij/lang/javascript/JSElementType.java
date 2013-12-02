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
package com.intellij.lang.javascript;

import java.lang.reflect.Constructor;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.types.PsiGenerator;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.psi.tree.IElementType;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 27, 2005
 * Time: 6:38:56 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSElementType extends IElementType implements PsiGenerator<JSElement>
{
	private Constructor<JSElement> constructor;
	private static final Logger LOG = Logger.getInstance("JSElementType.PsiGenerator");

	public JSElementType(@NonNls @NotNull String debugName)
	{
		this(debugName, true);
	}

	public JSElementType(@NonNls @NotNull String debugName, boolean register)
	{
		super(debugName, JavaScriptSupportLoader.JAVASCRIPT.getLanguage(), null, register);

		final StringBuilder builder = new StringBuilder("com.intellij.lang.javascript.psi.impl.JS");
		boolean doUp = false;
		for(int i = 0; i < debugName.length(); ++i)
		{
			final char ch = debugName.charAt(i);
			if(ch == '_')
			{
				doUp = true;
				continue;
			}

			builder.append(i == 0 || doUp ? Character.toUpperCase(ch) : Character.toLowerCase(ch));
			doUp = false;
		}

		builder.append("Impl");

		final String s = builder.toString();
		try
		{
			constructor = (Constructor<JSElement>) Class.forName(s).getConstructor(ASTNode.class);
		}
		catch(Exception e)
		{
		}
	}

	@SuppressWarnings({"HardCodedStringLiteral"})
	public String toString()
	{
		return "JS:" + super.toString();
	}

	public JSElement construct(ASTNode node)
	{
		if(constructor == null)
		{
			return null;
		}
		try
		{
			return constructor.newInstance(node);
		}
		catch(Exception ex)
		{
			LOG.error(ex);
			throw new RuntimeException(ex);
		}
	}
}

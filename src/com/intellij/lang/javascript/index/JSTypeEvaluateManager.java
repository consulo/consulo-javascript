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

package com.intellij.lang.javascript.index;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.lang.javascript.psi.resolve.BaseJSSymbolProcessor;
import com.intellij.openapi.components.ProjectComponent;
import com.intellij.openapi.project.Project;

/**
 * @by Maxim.Mossienko
 */
public class JSTypeEvaluateManager implements ProjectComponent
{

	public static JSTypeEvaluateManager getInstance(Project project)
	{
		return project.getComponent(JSTypeEvaluateManager.class);
	}

	@Override
	public void projectOpened()
	{
	}

	@Override
	public void projectClosed()
	{
	}

	@Override
	@NonNls
	@NotNull
	public String getComponentName()
	{
		return "JS.TypeEvaluateManager";
	}

	@Override
	public void initComponent()
	{
	}

	@Override
	public void disposeComponent()
	{
	}


	public static boolean isArrayType(String s)
	{
		if(s == null)
		{
			return false;
		}
		return s.endsWith("[]") ||
				s.indexOf('[') != -1 ||
				s.startsWith("Vector") && s.indexOf('<') != -1;
	}

	public static String getComponentType(String s)
	{
		if(s.endsWith("[]"))
		{
			return s.substring(0, s.length() - 2);
		}
		else
		{
			int i = s.indexOf('[');
			if(i != -1)
			{
				return s.substring(i + 1);
			}
			else if(s.startsWith("Vector"))
			{
				i = s.indexOf('<');
				int i2 = s.lastIndexOf('>');
				if(i2 == -1)
				{
					i2 = s.length();
				}
				if(i != -1 && i2 != -1 && i2 > i)
				{
					return s.substring(i + 1, i2);
				}
			}
		}
		return s;
	}

	public static String getInstanceNameByType(String className)
	{
		if("Document".equals(className))
		{
			return "HTMLDocument";
		}
		if("Element".equals(className))
		{
			return BaseJSSymbolProcessor.HTML_ELEMENT_TYPE_NAME;
		}
		if("DOMNode".equalsIgnoreCase(className))
		{
			return BaseJSSymbolProcessor.HTML_ELEMENT_TYPE_NAME;
		}
		return className;
	}
}

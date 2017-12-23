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

package com.intellij.lang.javascript.validation;

import java.util.LinkedHashMap;
import java.util.Map;

import org.jetbrains.annotations.Nullable;
import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import com.intellij.psi.PsiElement;
import com.intellij.psi.ResolveState;
import com.intellij.util.Function;

/**
 * @author Maxim.Mossienko
 *         Date: Jul 17, 2008
 *         Time: 9:50:49 PM
 */
public abstract class ImplementedMethodProcessor extends JSResolveUtil.CollectMethodsToImplementProcessor
{
	protected final JSClass myJsClass;

	public ImplementedMethodProcessor(final JSClass jsClass)
	{
		super(null, null);
		myJsClass = jsClass;
	}

	@Override
	protected boolean process(final ResolveProcessor processor)
	{
		Map<String, Object> functions = null;

		for(PsiElement _function : processor.getResults())
		{
			if(!(_function instanceof JSFunction))
			{
				continue;
			}
			final JSFunction function = (JSFunction) _function;
			final String name = function.getName();

			if(functions == null)
			{
				functions = collectAllVisibleClassFunctions(myJsClass, null, new Function<JSFunction, Boolean>()
				{
					@Override
					public Boolean fun(final JSFunction jsFunction)
					{
						final JSAttributeList attributeList = jsFunction.getAttributeList();
						PsiElement parentClass = JSResolveUtil.findParent(jsFunction);
						if((attributeList == null || attributeList.getAccessType() != JSAttributeList.AccessType.PUBLIC) && myJsClass != parentClass)
						{
							return Boolean.FALSE;
						}
						return Boolean.TRUE;
					}
				});
			}

			JSFunction o = findFunctionWithTheSameKind(functions, function, name);

			if(o == null)
			{
				if(function.isGetProperty() || function.isSetProperty())
				{
					JSVariable var = myJsClass.findFieldByName(name);
					if(var != null)
					{
						JSAttributeList attributeList = var.getAttributeList();
						if(attributeList != null && attributeList.getAccessType() == JSAttributeList.AccessType.PUBLIC)
						{
							continue; // implicit get and set methods
						}
					}
				}
				addNonimplementedFunction(function);
			}
			else
			{
				addImplementedFunction(function, o);
			}
		}
		return true;
	}

	protected void addImplementedFunction(final JSFunction interfaceFunction, final JSFunction implementationFunction)
	{
	}

	public static JSFunction findFunctionWithTheSameKind(final Map<String, Object> functions, final JSFunction function, final String name)
	{
		Object o = functions.get(name);
		if(o instanceof JSFunction && ((JSFunction) o).getKind() != function.getKind())
		{
			o = null;
		}
		else if(o instanceof JSFunction[])
		{
			final JSFunction[] jsFunctions = (JSFunction[]) o;
			o = null;
			for(JSFunction fun : jsFunctions)
			{
				if(fun.getKind() == function.getKind())
				{
					o = fun;
					break;
				}
			}
		}
		return (JSFunction) o;
	}

	public static Map<String, Object> collectAllVisibleClassFunctions(JSClass jsClass, Map<String, Object> _functions,
			final @Nullable Function<JSFunction, Boolean> filter)
	{
		final Map<String, Object> functions = _functions != null ? _functions : new LinkedHashMap<String, Object>();
		jsClass.processDeclarations(new ResolveProcessor(null)
		{
			{
				setToProcessHierarchy(true);
				setLocalResolve(true);
			}

			@Override
			public boolean execute(final PsiElement element, final ResolveState state)
			{
				if(element instanceof JSFunction)
				{
					final JSFunction function = (JSFunction) element;
					if(function.isConstructor())
					{
						return true; // SWC stubs have constructor methods :(
					}

					final JSAttributeList attributeList = function.getAttributeList();
					if(attributeList != null && attributeList.getAccessType() == JSAttributeList.AccessType.PRIVATE)
					{
						return true;
					}

					Boolean filterValue;
					if(filter != null && (filterValue = filter.fun(function)) != null && !filterValue.booleanValue())
					{
						return true;
					}
					final String s = function.getName();
					final Object function1 = functions.get(s);

					if(function1 == null)
					{
						functions.put(s, function);
					}
					else if(function1 instanceof JSFunction)
					{
						final JSFunction function2 = (JSFunction) function1;
						if(findFunctionWithTheSameKind(functions, function, s) == null)
						{
							functions.put(s, new JSFunction[]{
									function2,
									function
							});
						}
					}
				}
				return true;
			}
		}, ResolveState.initial(), jsClass, jsClass);
		return functions;
	}

	protected abstract void addNonimplementedFunction(final JSFunction function);
}

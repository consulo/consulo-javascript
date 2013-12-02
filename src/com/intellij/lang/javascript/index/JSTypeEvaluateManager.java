/*
 * Copyright 2000-2006 JetBrains s.r.o.
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

import gnu.trove.TIntHashSet;
import gnu.trove.TIntObjectHashMap;
import gnu.trove.TObjectIntHashMap;
import gnu.trove.TObjectIntProcedure;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.lang.javascript.psi.JSNamedElement;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.resolve.BaseJSSymbolProcessor;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.openapi.components.ProjectComponent;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.ResolveResult;

/**
 * @by Maxim.Mossienko
 */
public class JSTypeEvaluateManager implements ProjectComponent
{
	private TObjectIntHashMap<JSNamedElement> myTypeMap = new TObjectIntHashMap<JSNamedElement>(30);
	private TObjectIntHashMap<JSNamespace> myNsToSuperMap = new TObjectIntHashMap<JSNamespace>(50);
	private TIntObjectHashMap<List<JSNamespace>> myName2ElementMap = new TIntObjectHashMap<List<JSNamespace>>(30);
	private final JavaScriptIndex myIndex;
	private int myObjectNameIndex = -1;

	public static JSTypeEvaluateManager getInstance(Project project)
	{
		return project.getComponent(JSTypeEvaluateManager.class);
	}

	public JSTypeEvaluateManager(JavaScriptIndex index)
	{
		myIndex = index;
	}

	public void projectOpened()
	{
	}

	public void projectClosed()
	{
	}

	@NonNls
	@NotNull
	public String getComponentName()
	{
		return "JS.TypeEvaluateManager";
	}

	public void initComponent()
	{
	}

	public void disposeComponent()
	{
	}

	public void setElementType(JSNamedElement element, String type)
	{
		synchronized(myIndex)
		{
			myTypeMap.put(element, myIndex.getIndexOf(type));
		}
	}

	public String getElementType(PsiNamedElement element)
	{
		synchronized(myIndex)
		{
			if(element instanceof JSNamedElement)
			{
				int i = myTypeMap.get((JSNamedElement) element);
				if(i == 0)
				{
					return null;
				}
				return myIndex.getStringByIndex(i);
			}
			return null;
		}
	}

	public void setBaseType(JSNamespace namespace, int fqtypeIndex, int fqSuperIndex)
	{
		synchronized(myIndex)
		{
			if(myObjectNameIndex == -1)
			{
				myObjectNameIndex = myIndex.getIndexOf(JSResolveUtil.OBJECT_CLASS_NAME);
			}
			if(fqSuperIndex != myObjectNameIndex)
			{
				myNsToSuperMap.put(namespace, fqSuperIndex);
			}
			doAddNsWithType(fqtypeIndex, namespace);
		}
	}

	public void setBaseType(JSNamespace namespace, String fqtype, String superFQType)
	{
		final int of = myIndex.getIndexOf(superFQType);
		if(myObjectNameIndex == -1 && JSResolveUtil.OBJECT_CLASS_NAME.equals(superFQType))
		{
			myObjectNameIndex = of;
		}

		setBaseType(namespace, myIndex.getIndexOf(fqtype), of);
	}

	public interface NamespaceProcessor
	{
		boolean process(final JSNamespace superNs);
	}

	public boolean iterateTypeHierarchy(String fqTypeName, NamespaceProcessor processor)
	{
		if(fqTypeName == null || fqTypeName.length() == 0)
		{
			return true;
		}
		boolean result = true;

		synchronized(myIndex)
		{
			myIndex.getDefaultPackage();
			final int key = myIndex.getIndexOf(fqTypeName);

			List<JSNamespace> namedElements = myName2ElementMap.get(key);
			if(namedElements != null)
			{

				for(JSNamespace namespace : namedElements)
				{
					result &= doIterateTypeImpl(namespace, processor, new TIntHashSet());
				}
			}
			else
			{
				final JSPackage aPackage = JSResolveUtil.findPackageByText("Object", myIndex);
				if(aPackage != null)
				{
					for(JSNamespace superNs : aPackage.getInstances())
					{
						result &= processor.process(superNs);
					}
				}
			}
		}
		return result;
	}

	private boolean doIterateTypeImpl(final JSNamespace namespace, final NamespaceProcessor processor, TIntHashSet visited)
	{
		int superNameId = myNsToSuperMap.get(namespace);

		if(superNameId == 0)
		{
			if(namespace.getNameId() != myObjectNameIndex || !namespace.getQualifiedName(myIndex).equals(JSResolveUtil.OBJECT_CLASS_NAME))
			{
				superNameId = myObjectNameIndex;
			}
			else
			{
				return true;
			}
		}

		if(superNameId != 0 && !visited.contains(superNameId))
		{
			visited.add(superNameId);
			Collection<JSNamespace> superNamedElements = myName2ElementMap.get(superNameId);

			if(superNamedElements == null)
			{
				final JSPackage aPackage = JSResolveUtil.findPackageByText(myIndex.getStringByIndex(superNameId), myIndex);

				if(aPackage != null)
				{
					superNamedElements = aPackage.getInstances();
				}
				else
				{
					return true;
				}
			}

			boolean result = true;
			if(superNamedElements != null)
			{
				for(JSNamespace superNs : superNamedElements)
				{
					result &= processor.process(superNs);
					if(result)
					{
						result = doIterateTypeImpl(superNs, processor, visited);
					}
				}
			}

			return result;
		}
		return true;
	}

	public static boolean isTypeWithDefaultIndexedProperty(String s)
	{
		if(s == null)
		{
			return false;
		}
		return isArrayType(s) || s.endsWith("List") || s.endsWith("Map");
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

	public String evaluateType(JSReferenceExpression expr)
	{
		synchronized(myIndex)
		{
			for(ResolveResult r : expr.multiResolve(false))
			{
				final String type = getElementType((JSNamedElement) r.getElement());
				if(type != null)
				{
					return type;
				}
			}
			return null;
		}
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

	public void clear()
	{
		synchronized(myIndex)
		{
			myTypeMap.clear();
			myNsToSuperMap.clear();
			myName2ElementMap.clear();
			myObjectNameIndex = -1;
		}
	}

	public void removeNSInfo(final JSNamespace el)
	{
		synchronized(myIndex)
		{
			myNsToSuperMap.remove(el);

			remove(el);
		}
	}

	private void remove(final JSNamespace el)
	{
		final int ourId = el.getQualifiedNameId(myIndex);
		List<JSNamespace> list = myName2ElementMap.get(ourId);

		if(list != null)
		{
			list = new ArrayList<JSNamespace>(list);  // copy on write
			list.remove(el);
			if(list.size() == 0)
			{
				myName2ElementMap.remove(ourId);
			}
			else
			{
				myName2ElementMap.put(ourId, list);
			}
		}
	}

	public void removeElementInfo(final JSNamedElement el)
	{
		synchronized(myIndex)
		{
			myTypeMap.remove(el);
		}
	}

	public String getBaseType(final JSNamespace namespace)
	{
		synchronized(myIndex)
		{
			int i = myNsToSuperMap.get(namespace);
			if(i == 0)
			{
				return JSResolveUtil.OBJECT_CLASS_NAME;
			}
			return myIndex.getStringByIndex(i);
		}
	}

	private void doAddNsWithType(final int key, final JSNamespace superNs)
	{
		List<JSNamespace> namespaces = myName2ElementMap.get(key);

		if(namespaces == null)
		{
			namespaces = new ArrayList<JSNamespace>(1);
			myName2ElementMap.put(key, namespaces);
			namespaces.add(superNs);
		}
		else
		{
			if(namespaces.indexOf(superNs) == -1)
			{ // copy on write
				namespaces = new ArrayList<JSNamespace>(namespaces);
				myName2ElementMap.put(key, namespaces);
				namespaces.add(superNs);
			}
		}
	}

	public boolean iterateSubclasses(final String s, final NamespaceProcessor processor)
	{
		synchronized(myIndex)
		{
			myIndex.getDefaultPackage();
			return myNsToSuperMap.forEachEntry(new TObjectIntProcedure<JSNamespace>()
			{
				final int nameIndex = myIndex.getIndexOf(s);

				public boolean execute(final JSNamespace a, final int b)
				{
					if(nameIndex == b)
					{
						return processor.process(a);
					}
					return true;
				}
			});
		}
	}
}

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

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @by yole, maxim
 */
public class JSNamespace
{
	private JSNamespace myParent;
	private JSPackage myPackage;
	private Map<String, JSNamespace> myChildNamespaces;

	public JSNamespace(JSPackage _package)
	{
		myPackage = _package;
		if(_package != null)
		{
			_package.addInstance(this);
		}
	}

	public JSNamespace(final JSNamespace parent, final String nameId)
	{
		myParent = parent;
		myPackage = nameId == null ? parent.getPackage() : parent.getPackage().findSubPackage(nameId);
		myPackage.addInstance(this);
	}

	public void clear()
	{
		if(myChildNamespaces != null)
		{
			myChildNamespaces.clear();
		}
	}

	public final JSNamespace findChildNamespace(final String nameId)
	{
		if(myChildNamespaces == null)
		{
			return null;
		}

		return myChildNamespaces.get(nameId);
	}

	public JSNamespace getChildNamespace(final String nameId)
	{
		if(myChildNamespaces == null)
		{
			myChildNamespaces = new ConcurrentHashMap<String, JSNamespace>();
		}
		JSNamespace ns = myChildNamespaces.get(nameId);
		if(ns == null)
		{
			ns = new JSNamespace(this, nameId);
			myChildNamespaces.put(nameId, ns);
		}
		return ns;
	}

	public String getNameId()
	{
		return myPackage.getNameId();
	}

	public JSNamespace getParent()
	{
		return myParent;
	}

	public void enumerateNames(final SerializationContext context)
	{
		if(myParent != null)
		{
			context.addName(context.typeEvaluateManager.getBaseType(this));
		}

		if(myChildNamespaces != null)
		{
			for(JSNamespace o : myChildNamespaces.values())
			{
				o.enumerateNames(context);
			}
		}
	}


	private static int doEnumerateNS(JSNamespace ns, final SerializationContext context)
	{
		int i = context.myNameSpaces.get(ns);
		if(i == 0)
		{
			context.myNameSpaces.put(ns, i = context.myNameSpaces.size() + 1);
		}
		return i;
	}


	public String[] getIndices()
	{
		int count = 0;

		for(JSNamespace ns = this; ns.getParent() != null; ns = ns.getParent())
		{
			++count;
		}

		final String[] result = new String[count];
		for(JSNamespace ns = this; ns.getParent() != null; ns = ns.getParent())
		{
			result[--count] = ns.getNameId();
		}

		return result;
	}

	public String getQualifiedName(JavaScriptIndex index)
	{
		final StringBuilder buf = new StringBuilder();

		for(JSNamespace ns = this; ns.getParent() != null; ns = ns.getParent())
		{
			if(buf.length() > 0)
			{
				buf.insert(0, '.');
			}
			buf.insert(0,ns.getNameId());
		}

		return buf.toString();
	}

	public String getQualifiedNameId(JavaScriptIndex index)
	{
		if(myParent instanceof JSRootNamespace)
		{
			return getNameId();
		}
		return getQualifiedName(index);
	}

	public void invalidate(final JSTypeEvaluateManager typeEvaluateManager)
	{
		typeEvaluateManager.removeNSInfo(this);

		if(myChildNamespaces != null)
		{
			for(JSNamespace jsNamespace : myChildNamespaces.values())
			{
				jsNamespace.invalidate(typeEvaluateManager);
			}
		}

		myPackage.removeInstance(this);
	}

	public JSPackage getPackage()
	{
		return myPackage;
	}

	public boolean processDeclarations(final JavaScriptSymbolProcessor myProcessor)
	{
		return getRootNs().myEntry.processSymbolsInNs(myProcessor, this);
	}

	public final JSRootNamespace getRootNs()
	{
		JSNamespace ns = this;
		while(ns.myParent != null)
		{
			ns = ns.getParent();
		}
		return (JSRootNamespace) ns;
	}
}

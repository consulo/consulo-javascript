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

import gnu.trove.THashMap;
import gnu.trove.THashSet;

import java.util.Collection;
import java.util.Map;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * @by Maxim.Mosienko
 */
public class JSPackage
{
	private JSPackage myParent;
	private String myNameId;
	private Map<String, JSPackage> myChildPackages;
	private THashSet<JSNamespace> myInstances;
	private boolean myRootPackage;

	public JSPackage()
	{
		myParent = null;
		myRootPackage = true;
	}

	public JSPackage(@NotNull final JSPackage parent, final String nameId)
	{
		myParent = parent;
		myNameId = nameId;
	}

	synchronized void removeSubPackage(String nameId)
	{
		if(myChildPackages != null)
		{
			myChildPackages.remove(nameId);
		}
	}

	synchronized
	@NotNull
	JSPackage findSubPackage(String nameId)
	{
		if(myChildPackages == null)
		{
			myChildPackages = new THashMap<String, JSPackage>(5);
		}
		JSPackage jsPackage = myChildPackages.get(nameId);
		if(jsPackage == null)
		{
			jsPackage = new JSPackage(this, nameId);
			myChildPackages.put(nameId, jsPackage);
		}

		return jsPackage;
	}

	synchronized public void addInstance(@NotNull JSNamespace namespace)
	{
		final THashSet<JSNamespace> newInstances = new THashSet<JSNamespace>(myInstances != null ? myInstances.size() + 1 : 5);
		if(myInstances != null)
		{
			newInstances.addAll(myInstances);
		}
		newInstances.add(namespace);
		myInstances = newInstances;
	}

	synchronized public void removeInstance(@NotNull JSNamespace namespace)
	{
		if(myInstances != null)
		{
			final THashSet<JSNamespace> newInstances = new THashSet<JSNamespace>(myInstances.size());
			newInstances.addAll(myInstances);
			newInstances.remove(namespace);
			myInstances = newInstances;
			if(myInstances.size() == 0 && !myRootPackage)
			{
				myParent.removeSubPackage(myNameId);
			}
		}
	}

	synchronized public void clear()
	{
		myInstances = null;
		myChildPackages = null;
	}

	public String getNameId()
	{
		return myNameId;
	}

	public String getName(JavaScriptIndex index)
	{
		final StringBuilder buf = new StringBuilder();

		for(JSPackage ns = this; ns.getParent() != null; ns = ns.getParent())
		{
			if(buf.length() > 0)
			{
				buf.insert(0, '.');
			}
			buf.insert(0, ns.getNameId());
		}

		return buf.toString();
	}

	public synchronized boolean processDeclarations(final JavaScriptSymbolProcessor myProcessor)
	{
		if(myInstances != null)
		{
			for(JSNamespace ns : myInstances)
			{
				if(!ns.processDeclarations(myProcessor))
				{
					return false;
				}
			}
		}

		if(myProcessor instanceof PackageElementProcessor)
		{
			final PackageElementProcessor processor = (PackageElementProcessor) myProcessor;
			if(!processor.processPackages() || myChildPackages == null)
			{
				return true;
			}

			final String requiredNameId = myProcessor.getRequiredNameId();
			if(requiredNameId != null)
			{
				final JSPackage jsPackage = myChildPackages.get(requiredNameId);
				if(jsPackage != null)
				{
					return processor.processPackage(jsPackage);
				}
			}
			else
			{
				for(JSPackage jsPackage : myChildPackages.values())
				{
					if(!processor.processPackage(jsPackage))
					{
						return false;
					}
				}
			}
		}
		return true;
	}

	public synchronized
	@Nullable
	JSPackage findPackageWithNameId(final String packageIndex)
	{
		if(myChildPackages == null)
		{
			return null;
		}
		return myChildPackages.get(packageIndex);
	}

	public JSPackage getParent()
	{
		return myParent;
	}

	public Collection<JSNamespace> getInstances()
	{
		return myInstances;
	}

	public interface PackageElementProcessor extends JavaScriptSymbolProcessor
	{
		boolean processPackage(JSPackage _package);

		boolean processPackages();
	}
}

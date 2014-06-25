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

import gnu.trove.THashSet;
import gnu.trove.TIntObjectHashMap;
import gnu.trove.TIntObjectIterator;

import java.io.IOException;
import java.util.Collection;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * @by Maxim.Mosienko
 */
public class JSPackage
{
	private JSPackage myParent;
	private int myNameId;
	private TIntObjectHashMap<JSPackage> myChildPackages;
	private THashSet<JSNamespace> myInstances;
	private boolean myRootPackage;

	public JSPackage()
	{
		myParent = null;
		myNameId = -1;
		myRootPackage = true;
	}

	public JSPackage(@NotNull final JSPackage parent, final int nameId)
	{
		myParent = parent;
		myNameId = nameId;
	}

	synchronized void removeSubPackage(int nameId)
	{
		if(myChildPackages != null)
		{
			myChildPackages.remove(nameId);
		}
	}

	synchronized
	@NotNull
	JSPackage findSubPackage(int nameId)
	{
		if(myChildPackages == null)
		{
			myChildPackages = new TIntObjectHashMap<JSPackage>(5);
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

	public void serialize(final SerializationContext context) throws IOException
	{
		context.outputStream.writeInt(myNameId != -1 ? context.myNames.get(context.myIndex.getStringByIndex(myNameId)) : -1);
		final int packageId = context.myPackages.size() + 1;
		context.myPackages.put(this, packageId);
		context.outputStream.writeInt(packageId);

		if(myChildPackages != null)
		{
			final Object[] objects = myChildPackages.getValues(); // stable copy
			context.outputStream.writeInt(objects.length);

			for(Object o : objects)
			{
				((JSPackage) o).serialize(context);
			}
		}
		else
		{
			context.outputStream.writeInt(0);
		}
	}

	public void deserialize(final DeserializationContext context) throws IOException
	{
		myNameId = context.inputStream.readInt();
		final int packageId = context.inputStream.readInt();
		context.myPackages.put(packageId, this);
		final int childCount = context.inputStream.readInt();

		//context.myPackages.put(context.myPackages.size() + 1, this);

		if(childCount != 0)
		{
			myChildPackages = new TIntObjectHashMap<JSPackage>(childCount);
			for(int i = 0; i < childCount; ++i)
			{
				final JSPackage jsPackage = new JSPackage(this, -1);
				jsPackage.deserialize(context);
				myChildPackages.put(jsPackage.myNameId, jsPackage);
			}
		}
	}

	public int getNameId()
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
			buf.insert(0, index.getStringByIndex(ns.getNameId()));
		}

		return buf.toString();
	}

	public void enumerateNames(final SerializationContext context)
	{
		if(myNameId != -1)
		{
			context.addName(context.myIndex.getStringByIndex(myNameId));
		}
		if(myChildPackages != null)
		{
			final TIntObjectIterator<JSPackage> iterator = myChildPackages.iterator();

			while(iterator.hasNext())
			{
				iterator.advance();
				iterator.value().enumerateNames(context);
			}
		}
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

			final int requiredNameId = myProcessor.getRequiredNameId();
			if(requiredNameId != -1)
			{
				final JSPackage jsPackage = myChildPackages.get(requiredNameId);
				if(jsPackage != null)
				{
					return processor.processPackage(jsPackage);
				}
			}
			else
			{
				final TIntObjectIterator<JSPackage> tIntObjectIterator = myChildPackages.iterator();
				while(tIntObjectIterator.hasNext())
				{
					tIntObjectIterator.advance();

					if(!processor.processPackage(tIntObjectIterator.value()))
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
	JSPackage findPackageWithNameId(final int packageIndex)
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

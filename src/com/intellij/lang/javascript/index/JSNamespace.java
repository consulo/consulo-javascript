package com.intellij.lang.javascript.index;

import gnu.trove.TIntObjectHashMap;

import java.io.IOException;

/**
 * @by yole, maxim
 */
public class JSNamespace
{
	private JSNamespace myParent;
	private JSPackage myPackage;
	private TIntObjectHashMap<JSNamespace> myChildNamespaces;

	public JSNamespace(JSPackage _package)
	{
		myPackage = _package;
		if(_package != null)
		{
			_package.addInstance(this);
		}
	}

	public JSNamespace(final JSNamespace parent, final int nameId)
	{
		myParent = parent;
		myPackage = nameId == -1 ? parent.getPackage() : parent.getPackage().findSubPackage(nameId);
		myPackage.addInstance(this);
	}

	public void clear()
	{
		if(myChildNamespaces != null)
		{
			myChildNamespaces.clear();
		}
	}

	public final JSNamespace findChildNamespace(final int nameId)
	{
		if(myChildNamespaces == null)
		{
			return null;
		}

		return myChildNamespaces.get(nameId);
	}

	public JSNamespace getChildNamespace(final int nameId)
	{
		if(myChildNamespaces == null)
		{
			myChildNamespaces = new TIntObjectHashMap<JSNamespace>();
		}
		JSNamespace ns = myChildNamespaces.get(nameId);
		if(ns == null)
		{
			ns = new JSNamespace(this, nameId);
			myChildNamespaces.put(nameId, ns);
		}
		return ns;
	}

	public int getNameId()
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
			final Object[] objects = myChildNamespaces.getValues(); // stable copy
			for(Object o : objects)
			{
				((JSNamespace) o).enumerateNames(context);
			}
		}
	}

	public void write(SerializationContext context) throws IOException
	{
		context.outputStream.writeInt(doEnumerateNS(this, context));
		final int packageIndex = context.myPackages.get(myPackage);

		context.outputStream.writeInt(packageIndex);
		final int myBaseType = myParent != null ? context.myNames.get(context.typeEvaluateManager.getBaseType(this)) : -1;
		context.outputStream.writeInt(myBaseType);

		if(myChildNamespaces != null)
		{
			final Object[] objects = myChildNamespaces.getValues(); // stable copy
			context.outputStream.writeInt(objects.length);

			for(Object o : objects)
			{
				((JSNamespace) o).write(context);
			}
		}
		else
		{
			context.outputStream.writeInt(0);
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

	public void read(final DeserializationContext context, JSNamespace parent) throws IOException
	{
		final int nsIndex = context.inputStream.readInt();

		context.myNameSpaces.put(nsIndex, this);
		doRead(context, parent);
	}

	private void doRead(final DeserializationContext context, JSNamespace parent) throws IOException
	{
		myParent = parent;
		final int packageIndex = context.inputStream.readInt();
		myPackage = context.myPackages.get(packageIndex);
		assert myPackage != null;
		myPackage.addInstance(this);

		final int superNsType = context.inputStream.readInt();

		if(myParent != null)
		{
			context.typeEvaluateManager.setBaseType(this, getQualifiedNameId(context.index), superNsType);
		}

		int childCount = context.inputStream.readInt();

		while(childCount > 0)
		{
			JSNamespace item = new JSNamespace(null);
			item.read(context, this);
			if(myChildNamespaces == null)
			{
				myChildNamespaces = new TIntObjectHashMap<JSNamespace>(childCount);
			}
			myChildNamespaces.put(item.getNameId(), item);
			--childCount;
		}
	}

	public int[] getIndices()
	{
		int count = 0;

		for(JSNamespace ns = this; ns.getParent() != null; ns = ns.getParent())
		{
			++count;
		}

		final int[] result = new int[count];
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
			buf.insert(0, index.getStringByIndex(ns.getNameId()));
		}

		return buf.toString();
	}

	public int getQualifiedNameId(JavaScriptIndex index)
	{
		if(myParent instanceof JSRootNamespace)
		{
			return getNameId();
		}
		return index.getIndexOf(getQualifiedName(index));
	}

	public void invalidate(final JSTypeEvaluateManager typeEvaluateManager)
	{
		typeEvaluateManager.removeNSInfo(this);

		if(myChildNamespaces != null)
		{
			final Object[] objects = myChildNamespaces.getValues(); // stable copy

			for(Object o : objects)
			{
				((JSNamespace) o).invalidate(typeEvaluateManager);
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

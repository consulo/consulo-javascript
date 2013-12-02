package com.intellij.lang.javascript.psi.stubs.impl;

import java.io.IOException;

import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubBase;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 26, 2008
 *         Time: 7:11:48 PM
 */
abstract class JSNamedObjectStubBase<T extends PsiNamedElement> extends StubBase<T>
{
	protected final String myName;
	protected final int myFlags;

	protected JSNamedObjectStubBase(T clazz, final StubElement parent, final IStubElementType elementType)
	{
		super(parent, elementType);

		myName = clazz.getName();
		myFlags = buildFlags(clazz);
	}

	protected JSNamedObjectStubBase(String name, int flags, final StubElement parent, final IStubElementType elementType)
	{
		super(parent, elementType);

		myName = name;
		myFlags = flags;
	}

	protected abstract int buildFlags(final T clazz);

	protected JSNamedObjectStubBase(final StubInputStream dataStream, final StubElement parentStub, final IStubElementType elementType) throws
			IOException
	{
		super(parentStub, elementType);

		myName = readString(dataStream);
		myFlags = dataStream.readInt();
	}

	public void serialize(final StubOutputStream dataStream) throws IOException
	{
		writeString(myName, dataStream);
		dataStream.writeInt(myFlags);
	}

	public String getName()
	{
		return myName;
	}

	protected static String readString(final StubInputStream dataStream) throws IOException
	{
		final int i = dataStream.readInt();
		return i != -1 ? dataStream.stringFromId(i) : null;
	}

	protected static void writeString(String myTypeString, final StubOutputStream dataStream) throws IOException
	{
		dataStream.writeInt(myTypeString != null ? dataStream.getStringId(myTypeString) : -1);
	}
}
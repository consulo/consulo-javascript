package com.intellij.lang.javascript.psi.stubs.impl;

import java.io.IOException;

import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.stubs.JSVariableStubBase;
import com.intellij.lang.javascript.types.JSPackageStatementElementType;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 26, 2008
 *         Time: 11:29:19 PM
 */
public abstract class JSVariableStubBaseImpl<T extends JSVariable> extends JSQualifiedObjectStubBase<T> implements JSVariableStubBase<T>
{
	private String myTypeString;
	private static final int DEPRECATED_MASK = 1;
	public static final int CONST_MASK = 2;
	private static final int LOCAL_MASK = 4;
	static final int LAST_USED_MASK = LOCAL_MASK;
	private String myInitializerText;

	public JSVariableStubBaseImpl(T var, final StubElement parent, final IStubElementType elementType)
	{
		super(var, parent, elementType);
		myTypeString = var.getTypeString();
		myInitializerText = var.getInitializerText();
	}

	public JSVariableStubBaseImpl(final StubInputStream dataStream, final StubElement parentStub, final IStubElementType elementType) throws IOException
	{
		super(dataStream, parentStub, elementType);
		myTypeString = readString(dataStream);
		myInitializerText = readString(dataStream);
	}

	public JSVariableStubBaseImpl(final String name, int flags, String type, String initial, String qName, final StubElement parentStub,
			final IStubElementType elementType)
	{
		super(name, flags, qName, parentStub, elementType);
		myTypeString = type;
		myInitializerText = initial;
	}

	@Override
	protected int buildFlags(final T clazz)
	{
		return (clazz.isDeprecated() ? DEPRECATED_MASK : 0) | (clazz.isConst() ? CONST_MASK : clazz.isLocal() ? LOCAL_MASK : 0);
	}

	@Override
	public void serialize(final StubOutputStream dataStream) throws IOException
	{
		super.serialize(dataStream);

		writeString(myTypeString, dataStream);
		writeString(myInitializerText, dataStream);
	}

	@Override
	public String getTypeString()
	{
		return myTypeString;
	}

	@Override
	public boolean isDeprecated()
	{
		return (myFlags & DEPRECATED_MASK) != 0;
	}

	@Override
	public boolean isConst()
	{
		return (myFlags & CONST_MASK) != 0;
	}

	@Override
	public String getInitializerText()
	{
		return myInitializerText;
	}

	@Override
	public boolean isLocal()
	{
		return (myFlags & LOCAL_MASK) != 0;
	}

	@Override
	protected boolean doIndexName(final String name, final String fqn)
	{
		final IStubElementType stubType = getParentStub().getParentStub().getStubType();

		if(stubType instanceof JSPackageStatementElementType || stubType == null)
		{
			return true;
		}
		return false;
	}

	@Override
	protected boolean doIndexQualifiedName(final String name, final String fqn)
	{
		return doIndexName(name, fqn);
	}
}
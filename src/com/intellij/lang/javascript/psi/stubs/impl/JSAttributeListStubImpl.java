package com.intellij.lang.javascript.psi.stubs.impl;

import java.io.IOException;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.impl.JSAttributeListImpl;
import com.intellij.lang.javascript.psi.stubs.JSAttributeListStub;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.IndexSink;
import com.intellij.psi.stubs.StubBase;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 26, 2008
 *         Time: 11:29:19 PM
 */
public class JSAttributeListStubImpl extends StubBase<JSAttributeList> implements JSAttributeListStub
{
	private static final int VISIBILITY_TAG_SHIFT = 0;
	private static final int VISIBILITY_TAG_MASK = 0x3;
	private static final int OVERRIDE_PROPERTY_SHIFT = 2;
	private static final int STATIC_PROPERTY_SHIFT = 3;
	private static final int DYNAMIC_PROPERTY_SHIFT = 4;
	private static final int NATIVE_PROPERTY_SHIFT = 5;
	private static final int FINAL_PROPERTY_SHIFT = 6;
	private static final int VIRTUAL_PROPERTY_SHIFT = 7;

	private int myFlags;
	private String myNamespace;

	public JSAttributeListStubImpl(JSAttributeList clazz, final StubElement parent, final IStubElementType elementType)
	{
		super(parent, elementType);

		final JSAttributeList.AccessType accessType = clazz.getAccessType();
		final int ord = accessType.ordinal();

		myFlags |= (ord << VISIBILITY_TAG_SHIFT);

		for(JSAttributeList.ModifierType type : JSAttributeList.ModifierType.values())
		{
			setFlag(type, clazz.hasModifier(type));
		}

		myNamespace = clazz.getNamespace();
	}

	public JSAttributeListStubImpl(final StubInputStream dataStream, final StubElement parentStub, final IStubElementType elementType) throws
			IOException
	{
		super(parentStub, elementType);
		myFlags = dataStream.readInt();
		final int idx = dataStream.readInt();
		myNamespace = idx != -1 ? dataStream.stringFromId(idx) : null;
	}

	public JSAttributeListStubImpl(final StubElement parentStub, String namespace, JSAttributeList.AccessType accessType,
			JSAttributeList.ModifierType... modifiers)
	{
		super(parentStub, JSElementTypes.ATTRIBUTE_LIST);
		myFlags |= ((accessType != null ? accessType : JSAttributeList.AccessType.PACKAGE_LOCAL).ordinal() << VISIBILITY_TAG_SHIFT);

		for(JSAttributeList.ModifierType type : modifiers)
		{
			setFlag(type, true);
		}
		myNamespace = namespace;
	}

	@Override
	public JSAttributeList createPsi()
	{
		return new JSAttributeListImpl(this);
	}

	@Override
	public void index(final IndexSink sink)
	{
	}

	@Override
	public void serialize(final StubOutputStream dataStream) throws IOException
	{
		dataStream.writeInt(myFlags);
		dataStream.writeInt(myNamespace != null ? dataStream.getStringId(myNamespace) : -1);
	}

	@Override
	public JSAttributeList.AccessType getAccessType()
	{
		final int i = (myFlags >> VISIBILITY_TAG_SHIFT) & VISIBILITY_TAG_MASK;
		return types[i];
	}

	private static final JSAttributeList.AccessType[] types = JSAttributeList.AccessType.values();

	@Override
	public boolean hasModifier(final JSAttributeList.ModifierType modifier)
	{
		return ((myFlags >> getFlagShift(modifier)) & 0x1) != 0;
	}

	@Override
	public String getNamespace()
	{
		return myNamespace;
	}

	private static int getFlagShift(final JSAttributeList.ModifierType modifier)
	{
		int shift = -1;
		if(modifier == JSAttributeList.ModifierType.STATIC)
		{
			shift = STATIC_PROPERTY_SHIFT;
		}
		else if(modifier == JSAttributeList.ModifierType.DYNAMIC)
		{
			shift = DYNAMIC_PROPERTY_SHIFT;
		}
		else if(modifier == JSAttributeList.ModifierType.OVERRIDE)
		{
			shift = OVERRIDE_PROPERTY_SHIFT;
		}
		else if(modifier == JSAttributeList.ModifierType.NATIVE)
		{
			shift = NATIVE_PROPERTY_SHIFT;
		}
		else if(modifier == JSAttributeList.ModifierType.FINAL)
		{
			shift = FINAL_PROPERTY_SHIFT;
		}
		else if(modifier == JSAttributeList.ModifierType.VIRTUAL)
		{
			shift = VIRTUAL_PROPERTY_SHIFT;
		}
		if(shift == -1)
		{
			throw new IllegalArgumentException("Illegal modifier passed:" + modifier);
		}
		return shift;
	}

	private void setFlag(final JSAttributeList.ModifierType modifier, boolean value)
	{
		if(value)
		{
			myFlags |= (1 << getFlagShift(modifier));
		}
		else
		{
			myFlags &= ~(1 << getFlagShift(modifier));
		}
	}
}
package com.intellij.lang.javascript.psi.stubs.impl;

import java.io.IOException;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSReferenceList;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.impl.JSClassImpl;
import com.intellij.lang.javascript.psi.stubs.JSClassStub;
import com.intellij.lang.javascript.psi.stubs.JSImplementedInterfacesIndex;
import com.intellij.lang.javascript.psi.stubs.JSSuperClassIndex;
import com.intellij.psi.stubs.IndexSink;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubIndexKey;
import com.intellij.psi.stubs.StubInputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 26, 2008
 *         Time: 7:11:48 PM
 */
public class JSClassStubImpl extends JSQualifiedObjectStubBase<JSClass> implements JSClassStub
{
	public static final int INTERFACE_MASK = 1;
	private static final int DEPRECATED_MASK = 4;

	public JSClassStubImpl(JSClass clazz, final StubElement parent, final JSStubElementType elementType)
	{
		super(clazz, parent, elementType);
	}

	public JSClassStubImpl(String name, int flags, String qName, final StubElement parentStub)
	{
		super(name, flags, qName, parentStub, JSElementTypes.CLASS);
	}

	public JSClassStubImpl(final StubInputStream dataStream, final StubElement parentStub, final JSStubElementType elementType) throws IOException
	{
		super(dataStream, parentStub, elementType);
	}

	protected int buildFlags(final JSClass clazz)
	{
		return (clazz.isInterface() ? INTERFACE_MASK : 0) | (clazz.isDeprecated() ? DEPRECATED_MASK : 0);
	}

	public boolean isInterface()
	{
		return (myFlags & INTERFACE_MASK) != 0;
	}

	public boolean isDeprecated()
	{
		return (myFlags & DEPRECATED_MASK) != 0;
	}

	public String getName()
	{
		return myName;
	}

	public void index(final IndexSink sink)
	{
		super.index(sink);
		JSClass psi = getPsi();

		doIndex(sink, psi.getExtendsList(), JSSuperClassIndex.KEY);
		doIndex(sink, psi.getImplementsList(), JSImplementedInterfacesIndex.KEY);
	}

	private static void doIndex(IndexSink sink, JSReferenceList extendsList, StubIndexKey<String, JSClass> indexKey)
	{
		String[] referenceExpressions = extendsList != null ? extendsList.getReferenceTexts() : null;
		if(referenceExpressions == null)
		{
			return;
		}

		for(String s : referenceExpressions)
		{
			if(s != null)
			{
				sink.occurrence(indexKey, s.substring(s.lastIndexOf('.') + 1));
			}
		}
	}

	public JSClass createPsi()
	{
		return new JSClassImpl(this);
	}
}

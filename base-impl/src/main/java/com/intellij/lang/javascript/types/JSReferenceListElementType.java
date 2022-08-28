/*
 * Copyright 2000-2005 JetBrains s.r.o
 * Copyright 2013-2015 must-be.org
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

package com.intellij.lang.javascript.types;

import java.io.IOException;

import javax.annotation.Nonnull;

import consulo.language.psi.PsiElement;
import consulo.language.psi.stub.*;
import org.jetbrains.annotations.NonNls;
import consulo.language.ast.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSReferenceList;
import consulo.javascript.impl.language.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.impl.JSReferenceListImpl;
import com.intellij.lang.javascript.psi.stubs.JSReferenceListStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSReferenceListStubImpl;
import consulo.util.lang.StringUtil;
import consulo.util.collection.ArrayUtil;
import consulo.index.io.StringRef;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.psi.stub.JavaScriptIndexKeys;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 6, 2008
 *         Time: 7:51:24 PM
 */
public class JSReferenceListElementType extends JSStubElementType<JSReferenceListStub, JSReferenceList>
{
	public JSReferenceListElementType(@NonNls String name)
	{
		super(name);
	}

	@Override
	public void indexStub(@Nonnull JSReferenceListStub stub, @Nonnull IndexSink sink)
	{
		if(this == JSElementTypes.EXTENDS_LIST)
		{
			doIndex(sink, stub, JavaScriptIndexKeys.EXTENDS_INDEX);
		}
		else if(this == JSElementTypes.IMPLEMENTS_LIST)
		{
			doIndex(sink, stub, JavaScriptIndexKeys.IMPLEMENTED_INDEX);
		}
	}

	private static void doIndex(IndexSink sink, JSReferenceListStub stub, StubIndexKey<String, JSReferenceList> indexKey)
	{
		for(String s : stub.getReferenceTexts())
		{
			if(s != null)
			{
				sink.occurrence(indexKey, StringUtil.getShortName(s));
			}
		}
	}

	@Nonnull
	@Override
	public PsiElement createElement(@Nonnull ASTNode astNode)
	{
		return new JSReferenceListImpl(astNode);
	}

	@Override
	public JSReferenceList createPsi(@Nonnull JSReferenceListStub stub)
	{
		return new JSReferenceListImpl(stub, this);
	}

	@RequiredReadAction
	@Override
	public JSReferenceListStub createStub(@Nonnull JSReferenceList psi, StubElement parentStub)
	{
		String[] referenceTexts = psi.getReferenceTexts();
		return new JSReferenceListStubImpl(referenceTexts, parentStub, this);
	}

	@Override
	public void serialize(@Nonnull JSReferenceListStub stub, @Nonnull StubOutputStream dataStream) throws IOException
	{
		String[] referenceTexts = stub.getReferenceTexts();
		dataStream.writeVarInt(referenceTexts.length);
		for(String referenceText : referenceTexts)
		{
			dataStream.writeName(referenceText);
		}
	}

	@Nonnull
	@Override
	public JSReferenceListStub deserialize(@Nonnull StubInputStream dataStream, StubElement parentStub) throws IOException
	{
		int count = dataStream.readVarInt();
		String[] refs = ArrayUtil.newStringArray(count);
		for(int i = 0; i < count; i++)
		{
			refs[i] = StringRef.toString(dataStream.readName());
		}
		return new JSReferenceListStubImpl(refs, parentStub, this);
	}
}

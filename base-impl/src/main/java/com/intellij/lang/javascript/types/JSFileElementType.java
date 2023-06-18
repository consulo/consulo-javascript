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

import com.intellij.lang.javascript.psi.JSFile;
import consulo.index.io.StringRef;
import consulo.language.psi.stub.*;
import consulo.javascript.index.JavaScriptIndexer;
import consulo.javascript.psi.stubs.JSFileStub;
import consulo.javascript.impl.language.psi.stub.JSFileStubImpl;
import consulo.language.Language;
import consulo.language.psi.PsiFile;

import javax.annotation.Nonnull;
import java.io.IOException;

/**
 * @author peter
 */
public class JSFileElementType extends IStubFileElementType<JSFileStub>
{
	public JSFileElementType(final Language language)
	{
		super(language);
	}

	@Override
	public void indexStub(@Nonnull final JSFileStub stub, @Nonnull final IndexSink sink)
	{
		for(JavaScriptIndexer javaScriptIndexer : JavaScriptIndexer.EP_NAME.getExtensionList())
		{
			javaScriptIndexer.indexFile(stub, sink);
		}
	}

	@Override
	public StubBuilder getBuilder()
	{
		return new DefaultStubBuilder()
		{
			@Nonnull
			@Override
			protected StubElement createStubForFile(@Nonnull PsiFile file)
			{
				if(file instanceof JSFile)
				{
					return new JSFileStubImpl((JSFile) file, file.getName());
				}
				return super.createStubForFile(file);
			}
		};
	}

	@Override
	public void serialize(@Nonnull JSFileStub stub, @Nonnull StubOutputStream dataStream) throws IOException
	{
		dataStream.writeName(stub.getName());
	}

	@Nonnull
	@Override
	public JSFileStub deserialize(@Nonnull final StubInputStream dataStream, final StubElement parentStub) throws IOException
	{
		StringRef name = dataStream.readName();
		return new JSFileStubImpl(null, name);
	}

	@Nonnull
	@Override
	public String getExternalId()
	{
		return getLanguage() + ":" + toString();
	}

	@Override
	public int getStubVersion()
	{
		int[] version = new int[]{47};
		JavaScriptIndexer.EP_NAME.forEachExtensionSafe(javaScriptIndexer -> version[0] ++);
		return version[0];
	}
}

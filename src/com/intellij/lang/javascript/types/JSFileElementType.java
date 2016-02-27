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

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.Language;
import com.intellij.lang.javascript.index.JavaScriptIndexer;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.stubs.JSFileStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSFileStubImpl;
import com.intellij.psi.PsiFile;
import com.intellij.psi.StubBuilder;
import com.intellij.psi.stubs.DefaultStubBuilder;
import com.intellij.psi.stubs.IndexSink;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;
import com.intellij.psi.tree.IStubFileElementType;
import com.intellij.util.io.StringRef;

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
	public void indexStub(@NotNull final JSFileStub stub, @NotNull final IndexSink sink)
	{
		for(JavaScriptIndexer javaScriptIndexer : JavaScriptIndexer.EP_NAME.getExtensions())
		{
			javaScriptIndexer.indexFile(stub, sink);
		}
	}

	@Override
	public StubBuilder getBuilder()
	{
		return new DefaultStubBuilder()
		{
			@NotNull
			@Override
			protected StubElement createStubForFile(@NotNull PsiFile file)
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
	public void serialize(@NotNull JSFileStub stub, @NotNull StubOutputStream dataStream) throws IOException
	{
		dataStream.writeName(stub.getName());
	}

	@NotNull
	@Override
	public JSFileStub deserialize(@NotNull final StubInputStream dataStream, final StubElement parentStub) throws IOException
	{
		StringRef name = dataStream.readName();
		return new JSFileStubImpl(null, name);
	}

	@NotNull
	@Override
	public String getExternalId()
	{
		return getLanguage() + ":" + toString();
	}

	@Override
	public int getStubVersion()
	{
		int version = 39;
		for(JavaScriptIndexer javaScriptIndexer : JavaScriptIndexer.EP_NAME.getExtensions())
		{
			version += javaScriptIndexer.getVersion();
		}
		return version;
	}
}

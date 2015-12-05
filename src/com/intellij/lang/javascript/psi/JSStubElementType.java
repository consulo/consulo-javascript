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

/*
 * @author max
 */
package com.intellij.lang.javascript.psi;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.mustbe.consulo.javascript.lang.JavaScriptLanguage;
import com.intellij.lang.javascript.psi.stubs.JSStubElement;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.IndexSink;
import com.intellij.psi.tree.IElementTypeAsPsiFactory;

public abstract class JSStubElementType<StubT extends JSStubElement<PsiT>, PsiT extends JSElement> extends IStubElementType<StubT,
		PsiT> implements IElementTypeAsPsiFactory
{
	public JSStubElementType(@NonNls String debugName)
	{
		super(debugName, JavaScriptLanguage.INSTANCE);
	}

	@Override
	public String toString()
	{
		return "JS:" + super.toString();
	}

	@Override
	public void indexStub(@NotNull final StubT stub, @NotNull final IndexSink sink)
	{
	}

	@NotNull
	@Override
	public String getExternalId()
	{
		return toString();
	}
}
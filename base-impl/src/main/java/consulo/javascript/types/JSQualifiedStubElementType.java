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

package consulo.javascript.types;

import com.intellij.lang.javascript.psi.JSQualifiedNamedElement;
import consulo.javascript.impl.language.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSQualifiedStub;
import consulo.javascript.language.psi.stub.JavaScriptIndexKeys;
import consulo.language.psi.stub.IndexSink;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 03.05.2015
 */
public abstract class JSQualifiedStubElementType<StubT extends JSQualifiedStub<PsiT>, PsiT extends JSQualifiedNamedElement> extends JSStubElementType<StubT, PsiT>
{
	public JSQualifiedStubElementType(@NonNls String debugName)
	{
		super(debugName);
	}

	@Override
	public void indexStub(@Nonnull StubT stub, @Nonnull IndexSink sink)
	{
		final String name = stub.getName();
		final String fqn = stub.getQualifiedName();

		if(name != null && doIndexName(stub, name, fqn))
		{
			sink.occurrence(JavaScriptIndexKeys.ELEMENTS_BY_NAME, name);
		}

		if(fqn != null && doIndexQualifiedName(stub, name, fqn))
		{
			sink.occurrence(JavaScriptIndexKeys.ELEMENTS_BY_QNAME, fqn);
		}
	}

	protected boolean doIndexQualifiedName(StubT stub, final String name, final String fqn)
	{
		return true;
	}

	protected boolean doIndexName(StubT stub, final String name, final String fqn)
	{
		return true;
	}
}

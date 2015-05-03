package com.intellij.lang.javascript.types;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.lang.javascript.psi.JSQualifiedNamedElement;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSNameIndex;
import com.intellij.lang.javascript.psi.stubs.JSQualifiedElementIndex;
import com.intellij.lang.javascript.psi.stubs.JSQualifiedStub;
import com.intellij.psi.stubs.IndexSink;

/**
 * @author VISTALL
 * @since 03.05.2015
 */
public abstract class JSQualifiedStubElementType<StubT extends JSQualifiedStub<PsiT>, PsiT extends JSQualifiedNamedElement> extends
		JSStubElementType<StubT, PsiT>
{
	public JSQualifiedStubElementType(@NonNls String debugName)
	{
		super(debugName);
	}

	@Override
	public final void indexStub(@NotNull StubT stub, @NotNull IndexSink sink)
	{
		final String name = stub.getName();
		final String fqn = stub.getQualifiedName();

		if(name != null && doIndexName(stub, name, fqn))
		{
			sink.occurrence(JSNameIndex.KEY, name);
		}

		if(fqn != null && doIndexQualifiedName(stub, name, fqn))
		{
			sink.occurrence(JSQualifiedElementIndex.KEY, fqn);
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

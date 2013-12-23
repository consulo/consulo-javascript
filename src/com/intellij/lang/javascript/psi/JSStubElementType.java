/*
 * @author max
 */
package com.intellij.lang.javascript.psi;

import java.io.IOException;

import org.jetbrains.annotations.NonNls;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementType;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.stubs.JSStubElement;
import com.intellij.lang.javascript.types.PsiGenerator;
import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.IndexSink;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;

public abstract class JSStubElementType<StubT extends JSStubElement<PsiT>, PsiT extends JSElement> extends IStubElementType<StubT,
		PsiT> implements PsiGenerator
{
	private JSStubGenerator<StubT, PsiT> myStubGenerator;

	public JSStubElementType(@NonNls String debugName, JSStubGenerator<StubT, PsiT> stubGenerator)
	{
		super(debugName, JavaScriptSupportLoader.JAVASCRIPT.getLanguage());
		myStubGenerator = stubGenerator;
	}

	@Override
	public String toString()
	{
		return "JS:" + super.toString();
	}

	@Override
	public void indexStub(final StubT stub, final IndexSink sink)
	{
		stub.index(sink);
	}

	@Override
	public String getExternalId()
	{
		return toString();
	}

	@Override
	public PsiT createPsi(final StubT stub)
	{
		return stub.createPsi();
	}

	@Override
	public StubT createStub(final PsiT psi, final StubElement parentStub)
	{
		return myStubGenerator.newInstance(psi, parentStub, this);
	}

	@Override
	public void serialize(final StubT stub, final StubOutputStream dataStream) throws IOException
	{
		stub.serialize(dataStream);
	}

	@Override
	public StubT deserialize(final StubInputStream dataStream, final StubElement parentStub) throws IOException
	{
		return myStubGenerator.newInstance(dataStream, parentStub, this);
	}

	@Override
	public PsiElement construct(final ASTNode node)
	{
		return myStubGenerator.construct(node);
	}

	protected static abstract class JSStubGenerator<StubT extends JSStubElement<PsiT>, PsiT extends JSElement>
	{
		private JSElementType shadowType;

		public abstract StubT newInstance(final StubInputStream dataStream, final StubElement parentStub, final JSStubElementType<StubT,
				PsiT> elementType) throws IOException;

		public abstract StubT newInstance(final PsiT psi, final StubElement parentStub, final JSStubElementType<StubT, PsiT> elementType);

		public PsiElement construct(final ASTNode node)
		{
			JSElementType type = shadowType;
			if(type == null)
			{
				shadowType = type = new JSElementType(node.getElementType().toString().substring(3), false);
			}
			return type.construct(node);
		}
	}
}
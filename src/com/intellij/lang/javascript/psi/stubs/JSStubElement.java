package com.intellij.lang.javascript.psi.stubs;

import java.io.IOException;

import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.psi.stubs.IndexSink;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubOutputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 26, 2008
 *         Time: 9:39:31 PM
 */
public interface JSStubElement<T extends JSElement> extends StubElement<T>
{
	T createPsi();

	void index(final IndexSink sink);

	void serialize(final StubOutputStream dataStream) throws IOException;
}

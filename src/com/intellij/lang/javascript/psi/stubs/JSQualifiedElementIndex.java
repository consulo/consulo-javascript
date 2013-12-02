/*
 * @author max
 */
package com.intellij.lang.javascript.psi.stubs;

import com.intellij.lang.javascript.psi.JSQualifiedNamedElement;
import com.intellij.lang.javascript.types.JSFileElementType;
import com.intellij.psi.stubs.IntStubIndexExtension;
import com.intellij.psi.stubs.StubIndexKey;

public class JSQualifiedElementIndex extends IntStubIndexExtension<JSQualifiedNamedElement>
{
	public static final StubIndexKey<Integer, JSQualifiedNamedElement> KEY = StubIndexKey.createIndexKey("js.element.qualifiedName");
	private static final int VERSION = 2;

	public StubIndexKey<Integer, JSQualifiedNamedElement> getKey()
	{
		return KEY;
	}

	@Override
	public int getVersion()
	{
		return super.getVersion() + VERSION + JSFileElementType.VERSION;
	}
}
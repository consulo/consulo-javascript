/*
 * @author max
 */
package com.intellij.lang.javascript.psi.stubs;

import com.intellij.lang.javascript.psi.JSQualifiedNamedElement;
import com.intellij.lang.javascript.types.JSFileElementType;
import com.intellij.psi.stubs.StringStubIndexExtension;
import com.intellij.psi.stubs.StubIndexKey;

public class JSNameIndex extends StringStubIndexExtension<JSQualifiedNamedElement>
{
	public static final StubIndexKey<String, JSQualifiedNamedElement> KEY = StubIndexKey.createIndexKey("js.qualified.shortName");
	private static final int VERSION = 2;

	public StubIndexKey<String, JSQualifiedNamedElement> getKey()
	{
		return KEY;
	}

	@Override
	public int getVersion()
	{
		return super.getVersion() + VERSION + JSFileElementType.VERSION;
	}
}
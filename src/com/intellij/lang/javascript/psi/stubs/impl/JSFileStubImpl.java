package com.intellij.lang.javascript.psi.stubs.impl;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.stubs.JSFileStub;
import com.intellij.psi.stubs.PsiFileStubImpl;
import com.intellij.psi.tree.IStubFileElementType;

/**
 * @author VISTALL
 * @since 19.07.2015
 */
public class JSFileStubImpl extends PsiFileStubImpl<JSFile> implements JSFileStub
{
	public JSFileStubImpl(JSFile file)
	{
		super(file);
	}

	@Override
	public IStubFileElementType getType()
	{
		return JSElementTypes.FILE;
	}
}

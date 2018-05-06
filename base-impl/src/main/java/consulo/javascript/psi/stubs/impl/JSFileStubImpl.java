package consulo.javascript.psi.stubs.impl;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSFile;
import consulo.javascript.psi.stubs.JSFileStub;
import com.intellij.psi.stubs.PsiFileStubImpl;
import com.intellij.psi.tree.IStubFileElementType;
import com.intellij.util.io.StringRef;

/**
 * @author VISTALL
 * @since 19.07.2015
 */
public class JSFileStubImpl extends PsiFileStubImpl<JSFile> implements JSFileStub
{
	private StringRef myName;

	public JSFileStubImpl(@Nullable JSFile file, @Nonnull String name)
	{
		super(file);
		myName = StringRef.fromString(name);
	}

	public JSFileStubImpl(@Nullable JSFile file, @Nonnull StringRef name)
	{
		super(file);
		myName = name;
	}

	@Override
	public IStubFileElementType getType()
	{
		return JSElementTypes.FILE;
	}

	@Nonnull
	@Override
	public String getName()
	{
		return StringRef.toString(myName);
	}
}

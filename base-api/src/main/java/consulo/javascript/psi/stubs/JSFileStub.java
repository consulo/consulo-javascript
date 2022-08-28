package consulo.javascript.psi.stubs;

import com.intellij.lang.javascript.psi.JSFile;
import consulo.language.psi.stub.PsiFileStub;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 19.07.2015
 */
public interface JSFileStub extends PsiFileStub<JSFile>
{
	@Nonnull
	String getName();
}

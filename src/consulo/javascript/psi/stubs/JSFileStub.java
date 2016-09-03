package consulo.javascript.psi.stubs;

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.psi.stubs.PsiFileStub;

/**
 * @author VISTALL
 * @since 19.07.2015
 */
public interface JSFileStub extends PsiFileStub<JSFile>
{
	@NotNull
	String getName();
}

package consulo.javascript.psi.stubs;

import com.intellij.lang.javascript.psi.JSFile;
import consulo.language.psi.stub.PsiFileStub;

import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2015-07-19
 */
public interface JSFileStub extends PsiFileStub<JSFile> {
    @Nonnull
    String getName();
}

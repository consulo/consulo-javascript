package consulo.javascript.impl.language.psi.stub;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSFile;
import consulo.index.io.StringRef;
import consulo.javascript.psi.stubs.JSFileStub;
import consulo.language.psi.stub.IStubFileElementType;
import consulo.language.psi.stub.PsiFileStubImpl;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * @author VISTALL
 * @since 19.07.2015
 */
public class JSFileStubImpl extends PsiFileStubImpl<JSFile> implements JSFileStub {
    private StringRef myName;

    public JSFileStubImpl(@Nullable JSFile file, @Nonnull String name) {
        super(file);
        myName = StringRef.fromString(name);
    }

    public JSFileStubImpl(@Nullable JSFile file, @Nonnull StringRef name) {
        super(file);
        myName = name;
    }

    @Override
    public IStubFileElementType getType() {
        return JSElementTypes.FILE;
    }

    @Nonnull
    @Override
    public String getName() {
        return StringRef.toString(myName);
    }
}

package consulo.javascript.impl.language.psi.stub;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSFile;
import consulo.index.io.StringRef;
import consulo.javascript.module.resolve.JSModuleExport;
import consulo.javascript.psi.stubs.JSFileStub;
import consulo.language.psi.stub.IStubFileElementType;
import consulo.language.psi.stub.PsiFileStubImpl;

import org.jspecify.annotations.Nullable;

import java.util.List;

/**
 * @author VISTALL
 * @since 19.07.2015
 */
public class JSFileStubImpl extends PsiFileStubImpl<JSFile> implements JSFileStub {
    private StringRef myName;
    private final List<JSModuleExport> myExports;

    public JSFileStubImpl(@Nullable JSFile file, String name) {
        this(file, StringRef.fromString(name), List.of());
    }

    public JSFileStubImpl(@Nullable JSFile file, StringRef name) {
        this(file, name, List.of());
    }

    public JSFileStubImpl(@Nullable JSFile file, StringRef name, List<JSModuleExport> exports) {
        super(file);
        myName = name;
        myExports = List.copyOf(exports);
    }

    @Override
    public IStubFileElementType getType() {
        return JSElementTypes.FILE;
    }

    @Override
    public String getName() {
        return StringRef.toString(myName);
    }

    @Override
    public List<JSModuleExport> getExports() {
        return myExports;
    }
}

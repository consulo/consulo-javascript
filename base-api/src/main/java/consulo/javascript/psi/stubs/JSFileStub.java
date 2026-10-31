package consulo.javascript.psi.stubs;

import consulo.javascript.module.resolve.JSModuleExport;

import java.util.List;

import com.intellij.lang.javascript.psi.JSFile;
import consulo.language.psi.stub.PsiFileStub;


/**
 * @author VISTALL
 * @since 2015-07-19
 */
public interface JSFileStub extends PsiFileStub<JSFile> {
    String getName();

    /**
     * The exports of an ES module, in the order of the file.
     */
    default List<JSModuleExport> getExports() {
        return List.of();
    }
}

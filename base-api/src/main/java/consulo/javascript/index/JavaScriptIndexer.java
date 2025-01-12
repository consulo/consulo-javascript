package consulo.javascript.index;

import consulo.annotation.component.ComponentScope;
import consulo.annotation.component.ExtensionAPI;
import consulo.component.extension.ExtensionPointName;
import consulo.javascript.psi.stubs.JSFileStub;
import consulo.language.psi.stub.IndexSink;
import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 19.07.2015
 */
@ExtensionAPI(ComponentScope.APPLICATION)
public abstract class JavaScriptIndexer {
    public static final ExtensionPointName<JavaScriptIndexer> EP_NAME = ExtensionPointName.create(JavaScriptIndexer.class);

    public void indexFile(@Nonnull JSFileStub fileStub, @Nonnull final IndexSink sink) {
    }

    public int getVersion() {
        return 0;
    }
}

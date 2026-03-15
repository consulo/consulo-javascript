package consulo.javascript.index;

import consulo.annotation.component.ComponentScope;
import consulo.annotation.component.ExtensionAPI;
import consulo.component.extension.ExtensionPointName;
import consulo.javascript.psi.stubs.JSFileStub;
import consulo.language.psi.stub.IndexSink;

/**
 * @author VISTALL
 * @since 2015-07-19
 */
@ExtensionAPI(ComponentScope.APPLICATION)
public abstract class JavaScriptIndexer {
    public static final ExtensionPointName<JavaScriptIndexer> EP_NAME = ExtensionPointName.create(JavaScriptIndexer.class);

    public void indexFile(JSFileStub fileStub, IndexSink sink) {
    }

    public int getVersion() {
        return 0;
    }
}

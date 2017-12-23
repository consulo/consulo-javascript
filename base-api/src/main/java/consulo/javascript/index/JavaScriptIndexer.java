package consulo.javascript.index;

import org.jetbrains.annotations.NotNull;
import consulo.javascript.psi.stubs.JSFileStub;
import com.intellij.openapi.extensions.ExtensionPointName;
import com.intellij.psi.stubs.IndexSink;

/**
 * @author VISTALL
 * @since 19.07.2015
 */
public abstract class JavaScriptIndexer
{
	public static final ExtensionPointName<JavaScriptIndexer> EP_NAME = ExtensionPointName.create("consulo.javascript.indexer");

	public void indexFile(@NotNull JSFileStub fileStub, @NotNull final IndexSink sink)
	{
	}

	public int getVersion()
	{
		return 0;
	}
}

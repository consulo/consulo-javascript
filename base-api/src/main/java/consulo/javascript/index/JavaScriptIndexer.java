package consulo.javascript.index;

import javax.annotation.Nonnull;

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

	public void indexFile(@Nonnull JSFileStub fileStub, @Nonnull final IndexSink sink)
	{
	}

	public int getVersion()
	{
		return 0;
	}
}

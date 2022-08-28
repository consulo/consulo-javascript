package consulo.javascript.index;

import consulo.component.extension.ExtensionPointName;
import consulo.javascript.psi.stubs.JSFileStub;
import consulo.language.psi.stub.IndexSink;

import javax.annotation.Nonnull;
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

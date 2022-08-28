package consulo.javascript.jsx;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptFileType;
import consulo.virtualFileSystem.fileType.FileTypeConsumer;
import consulo.virtualFileSystem.fileType.FileTypeFactory;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2020-06-14
 */
@ExtensionImpl
public class JSXFileTypeFactory extends FileTypeFactory
{
	@Override
	public void createFileTypes(@Nonnull FileTypeConsumer consumer)
	{
		consumer.consume(JavaScriptFileType.INSTANCE, "jsx");
	}
}

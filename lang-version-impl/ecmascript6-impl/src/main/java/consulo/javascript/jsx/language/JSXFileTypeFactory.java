package consulo.javascript.jsx.language;

import com.intellij.openapi.fileTypes.FileTypeConsumer;
import com.intellij.openapi.fileTypes.FileTypeFactory;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2019-12-16
 */
public class JSXFileTypeFactory extends FileTypeFactory
{
	@Override
	public void createFileTypes(@Nonnull FileTypeConsumer fileTypeConsumer)
	{
		fileTypeConsumer.consume(JSXFileType.INSTANCE);
	}
}

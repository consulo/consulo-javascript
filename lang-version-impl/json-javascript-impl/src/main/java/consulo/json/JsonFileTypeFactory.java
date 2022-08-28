package consulo.json;

import javax.annotation.Nonnull;
import consulo.virtualFileSystem.fileType.FileTypeConsumer;
import consulo.virtualFileSystem.fileType.FileTypeFactory;

/**
 * @author VISTALL
 * @since 05.03.2015
 */
public class JsonFileTypeFactory extends FileTypeFactory
{
	@Override
	public void createFileTypes(@Nonnull FileTypeConsumer consumer)
	{
		consumer.consume(JsonFileType.INSTANCE);
	}
}

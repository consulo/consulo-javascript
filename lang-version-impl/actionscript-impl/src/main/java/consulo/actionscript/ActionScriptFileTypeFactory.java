package consulo.actionscript;

import javax.annotation.Nonnull;

import consulo.virtualFileSystem.fileType.FileTypeConsumer;
import consulo.virtualFileSystem.fileType.FileTypeFactory;

/**
 * @author VISTALL
 * @since 06.04.2015
 */
public class ActionScriptFileTypeFactory extends FileTypeFactory
{
	@Override
	public void createFileTypes(@Nonnull FileTypeConsumer consumer)
	{
		consumer.consume(ActionScriptFileType.INSTANCE);
	}
}

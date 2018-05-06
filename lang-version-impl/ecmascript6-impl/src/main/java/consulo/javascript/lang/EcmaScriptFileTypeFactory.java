package consulo.javascript.lang;

import javax.annotation.Nonnull;
import com.intellij.openapi.fileTypes.FileTypeConsumer;
import com.intellij.openapi.fileTypes.FileTypeFactory;

/**
 * @author VISTALL
 * @since 05.03.2015
 */
public class EcmaScriptFileTypeFactory extends FileTypeFactory
{
	@Override
	public void createFileTypes(@Nonnull FileTypeConsumer consumer)
	{
		consumer.consume(EcmaScriptFileType.INSTANCE, "es;js2");
	}
}

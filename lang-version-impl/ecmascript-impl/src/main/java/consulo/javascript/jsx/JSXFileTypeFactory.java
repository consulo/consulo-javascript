package consulo.javascript.jsx;

import com.intellij.lang.javascript.JavaScriptFileType;
import com.intellij.openapi.fileTypes.FileTypeConsumer;
import com.intellij.openapi.fileTypes.FileTypeFactory;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2020-06-14
 */
public class JSXFileTypeFactory extends FileTypeFactory
{
	@Override
	public void createFileTypes(@Nonnull FileTypeConsumer consumer)
	{
		consumer.consume(JavaScriptFileType.INSTANCE, "jsx");
	}
}

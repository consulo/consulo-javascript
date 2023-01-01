package consulo.json.lang;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.impl.language.JavaScriptQuoteHandler;
import consulo.json.JsonFileType;
import consulo.virtualFileSystem.fileType.FileType;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 29-Aug-22
 */
@ExtensionImpl
public class JsonQuoteHandler extends JavaScriptQuoteHandler
{
	@Nonnull
	@Override
	public FileType getFileType()
	{
		return JsonFileType.INSTANCE;
	}
}

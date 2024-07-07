package consulo.javascript.impl.ide.completion;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.editor.completion.SkipAutopopupInStrings;

import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 29-Aug-22
 */
@ExtensionImpl
public class JavaScriptSkipAutopopupInStrings extends SkipAutopopupInStrings
{
	@Nonnull
	@Override
	public Language getLanguage()
	{
		return JavaScriptLanguage.INSTANCE;
	}
}

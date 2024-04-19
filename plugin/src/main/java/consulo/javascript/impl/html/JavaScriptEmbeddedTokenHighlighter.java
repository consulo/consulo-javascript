package consulo.javascript.impl.html;

import com.intellij.xml.highlighter.EmbeddedTokenHighlighter;
import consulo.annotation.component.ExtensionImpl;
import consulo.colorScheme.TextAttributesKey;
import consulo.javascript.ide.hightlight.JavaScriptHighlighter;
import consulo.language.ast.IElementType;
import consulo.util.collection.MultiMap;
import consulo.xml.lang.xml.XMLLanguage;
import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2024-04-19
 */
@ExtensionImpl
public class JavaScriptEmbeddedTokenHighlighter implements EmbeddedTokenHighlighter
{
	@Nonnull
	@Override
	public MultiMap<IElementType, TextAttributesKey> getEmbeddedTokenAttributes(@Nonnull XMLLanguage language)
	{
		MultiMap<IElementType, TextAttributesKey> keys = MultiMap.createLinked();
		JavaScriptHighlighter.storeDefaults(keys);
		return keys;
	}
}

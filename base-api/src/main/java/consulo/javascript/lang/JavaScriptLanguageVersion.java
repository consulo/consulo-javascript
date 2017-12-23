package consulo.javascript.lang;

import java.util.Set;

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.Language;
import consulo.lang.LanguageVersion;
import consulo.lang.LanguageVersionWithParsing;

/**
 * @author VISTALL
 * @since 23-Dec-17
 */
public abstract class JavaScriptLanguageVersion extends LanguageVersion implements LanguageVersionWithParsing
{
	public JavaScriptLanguageVersion(@NotNull String id, @NotNull String name, @NotNull Language language, String... mimeTypes)
	{
		super(id, name, language, mimeTypes);
	}

	@NotNull
	public abstract Set<JavaScriptFeature> getFeatures();

	@NotNull
	public String getPresentableName()
	{
		return getName();
	}
}

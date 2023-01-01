package consulo.javascript.language;

import consulo.javascript.language.JavaScriptFeature;
import consulo.language.Language;
import consulo.language.version.LanguageVersion;
import consulo.language.version.LanguageVersionWithParsing;

import javax.annotation.Nonnull;
import java.util.Set;

/**
 * @author VISTALL
 * @since 23-Dec-17
 */
public abstract class JavaScriptLanguageVersion extends LanguageVersion implements LanguageVersionWithParsing
{
	public JavaScriptLanguageVersion(@Nonnull String id, @Nonnull String name, @Nonnull Language language, String... mimeTypes)
	{
		super(id, name, language, mimeTypes);
	}

	@Nonnull
	public abstract Set<JavaScriptFeature> getFeatures();

	@Nonnull
	public String getPresentableName()
	{
		return getName();
	}
}

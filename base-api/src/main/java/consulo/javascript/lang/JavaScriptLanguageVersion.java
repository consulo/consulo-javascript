package consulo.javascript.lang;

import java.util.Set;

import javax.annotation.Nonnull;

import com.intellij.lang.Language;
import consulo.lang.LanguageVersion;
import consulo.lang.LanguageVersionWithParsing;

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

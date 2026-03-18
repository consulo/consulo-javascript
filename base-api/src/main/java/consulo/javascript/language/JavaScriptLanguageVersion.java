package consulo.javascript.language;

import consulo.language.Language;
import consulo.language.version.LanguageVersion;
import consulo.language.version.LanguageVersionWithParsing;


import java.util.Set;

/**
 * @author VISTALL
 * @since 2017-12-23
 */
public abstract class JavaScriptLanguageVersion extends LanguageVersion implements LanguageVersionWithParsing {
    public JavaScriptLanguageVersion(String id, String name, Language language, String... mimeTypes) {
        super(id, name, language, mimeTypes);
    }

    public abstract Set<JavaScriptFeature> getFeatures();

    public String getPresentableName() {
        return getName();
    }

    public boolean supportsDefaultCompletion() {
        return true;
    }
}

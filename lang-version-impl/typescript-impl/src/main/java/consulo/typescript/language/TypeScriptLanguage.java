package consulo.typescript.language;

import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;

/**
 * TypeScript language definition. Extends JavaScript as its base language,
 * so {@code TypeScriptLanguage.INSTANCE.isKindOf(JavaScriptLanguage.INSTANCE)} returns true.
 *
 * @author VISTALL
 * @since 2026-03-18
 */
public class TypeScriptLanguage extends Language {
    public static final TypeScriptLanguage INSTANCE = new TypeScriptLanguage();

    private TypeScriptLanguage() {
        super(JavaScriptLanguage.INSTANCE, "TypeScript", "application/typescript");
    }

    @Override
    public boolean isCaseSensitive() {
        return true;
    }
}

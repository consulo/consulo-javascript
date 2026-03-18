package consulo.typescript.version;

import consulo.javascript.ecmascript.lang.BaseEcmaScriptJavaScriptVersion;
import consulo.javascript.language.JavaScriptFeature;
import consulo.javascript.language.StandardJavaScriptVersion;
import consulo.javascript.ide.hightlight.JavaScriptHighlighter;
import consulo.language.editor.highlight.SyntaxHighlighter;
import consulo.language.lexer.Lexer;
import consulo.language.parser.PsiParser;
import consulo.typescript.language.TypeScriptFeature;
import consulo.typescript.language.TypeScriptLanguage;
import consulo.typescript.language.impl.syntax.TypeScriptSyntaxParser;
import consulo.typescript.language.impl.syntax._TypeScriptLexer;

import java.util.Collections;
import java.util.EnumSet;
import java.util.Set;

/**
 * Base class for all TypeScript language version implementations.
 * Versions form a chain (TS 6.0 extends TS 5.9 extends ... extends TS 1.0 extends this),
 * each adding its own {@link TypeScriptFeature}s via {@link #addTypeScriptFeature}.
 *
 * @author VISTALL
 * @since 2026-03-17
 */
public abstract class BaseTypeScriptLanguageVersion extends BaseEcmaScriptJavaScriptVersion implements StandardJavaScriptVersion {
    private final Set<TypeScriptFeature> myTypeScriptFeatures = EnumSet.noneOf(TypeScriptFeature.class);

    protected BaseTypeScriptLanguageVersion(String id) {
        super(id, TypeScriptLanguage.INSTANCE, "application/typescript");

        // TypeScript is a superset of JavaScript
        for (JavaScriptFeature feature : JavaScriptFeature.values()) {
            addFeature(feature);
        }
    }

    public static BaseTypeScriptLanguageVersion getLatest() {
        return TypeScriptLanguage.INSTANCE.findVersionByClass(TypeScript60LanguageVersion.class);
    }

    protected void addTypeScriptFeature(TypeScriptFeature feature) {
        myTypeScriptFeatures.add(feature);
    }

    public Set<TypeScriptFeature> getTypeScriptFeatures() {
        return Collections.unmodifiableSet(myTypeScriptFeatures);
    }

    public boolean isFeatureSupported(TypeScriptFeature feature) {
        return myTypeScriptFeatures.contains(feature);
    }

    @Override
    public boolean supportsDefaultCompletion() {
        return false;
    }

    @Override
    public PsiParser createParser() {
        return new TypeScriptSyntaxParser();
    }

    @Override
    public Lexer createLexer() {
        return new _TypeScriptLexer();
    }

    @Override
    public SyntaxHighlighter getSyntaxHighlighter() {
        return new JavaScriptHighlighter(_TypeScriptLexer::new);
    }
}

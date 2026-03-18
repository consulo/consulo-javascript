package consulo.typescript.language;

import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.ast.IElementType;
import org.jspecify.annotations.NonNull;

/**
 * @author VISTALL
 * @since 2026-03-17
 */
public class TypeScriptTokenType extends IElementType {
    public TypeScriptTokenType(@NonNull String debugName) {
        super(debugName, JavaScriptLanguage.INSTANCE);
    }
}

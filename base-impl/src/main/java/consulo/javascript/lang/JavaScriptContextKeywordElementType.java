package consulo.javascript.lang;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.intellij.lang.javascript.JSElementType;
import consulo.language.ast.IElementType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * @author VISTALL
 * @since 2019-12-14
 */
public class JavaScriptContextKeywordElementType extends JSElementType {
    private static final BiMap<String, IElementType> ourCache = HashBiMap.create();

    @Nullable
    public static IElementType getKeywordByText(String text) {
        return ourCache.get(text);
    }

    public static boolean containsKeyword(@Nonnull IElementType e) {
        return ourCache.inverse().containsKey(e);
    }

    public JavaScriptContextKeywordElementType(@Nonnull String id, @Nonnull String keyword) {
        super(id);

        ourCache.put(keyword, this);
    }
}

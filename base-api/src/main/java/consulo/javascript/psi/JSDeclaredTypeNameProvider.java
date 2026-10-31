package consulo.javascript.psi;

import consulo.annotation.access.RequiredReadAction;
import org.jspecify.annotations.Nullable;

/**
 * A declaration whose declared type names a class: the type of a variable, the return type of a function - normalized
 * from a type text like TypeScript's ({@code HTMLElement | null} is {@code HTMLElement}, {@code string} is
 * {@code String}, {@code T[]} is {@code Array}). The JavaScript type evaluation asks it instead of the raw type text
 * ({@code document.getElementById('a').focus()}).
 *
 * @since 2026-10-31
 */
public interface JSDeclaredTypeNameProvider {
    /**
     * The name of the class of the declared type (of the return type for a function), null when it names none.
     */
    @Nullable
    @RequiredReadAction
    String getDeclaredTypeName();
}

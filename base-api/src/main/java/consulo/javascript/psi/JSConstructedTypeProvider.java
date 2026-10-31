package consulo.javascript.psi;

import consulo.annotation.access.RequiredReadAction;
import org.jspecify.annotations.Nullable;

/**
 * A declaration that knows the type its {@code new} creates: a TypeScript variable typed by a constructor type
 * ({@code declare var Date: DateConstructor}, {@code declare var Image: { new (): HTMLImageElement }}). The JavaScript
 * type evaluation of {@code new X()} asks it instead of using the declared type of X.
 *
 * @since 2026-10-31
 */
public interface JSConstructedTypeProvider {
    /**
     * The qualified name of the type of {@code new X()}, null when unknown.
     */
    @Nullable
    @RequiredReadAction
    String getConstructedTypeName();
}

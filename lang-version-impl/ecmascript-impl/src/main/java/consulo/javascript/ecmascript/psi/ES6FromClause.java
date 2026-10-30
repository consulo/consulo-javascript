package consulo.javascript.ecmascript.psi;

import org.jspecify.annotations.Nullable;

/**
 * {@code from '...'} of an import / export declaration: IntelliJ's API.
 *
 * @since 2026-10-31
 */
public interface ES6FromClause {
    /**
     * The module specifier with its quotes ({@code '@hotwired/stimulus'}).
     */
    @Nullable
    String getReferenceText();
}

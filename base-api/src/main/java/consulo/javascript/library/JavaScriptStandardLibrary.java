package consulo.javascript.library;

import consulo.annotation.component.ComponentScope;
import consulo.annotation.component.ServiceAPI;
import consulo.application.Application;
import consulo.language.version.LanguageVersion;
import consulo.virtualFileSystem.VirtualFile;
import org.jspecify.annotations.Nullable;

import java.util.List;
import java.util.Set;

/**
 * The standard library declarations of TypeScript ({@code lib.es5.d.ts}, {@code lib.dom.d.ts}, ...) bundled with the
 * plugin, and the libraries a module sees from a directory of them: an SDK root (the Web SDK, a Node.js SDK) or
 * {@code node_modules/typescript/lib}.
 *
 * @since 2026-10-31
 */
@ServiceAPI(ComponentScope.APPLICATION)
public abstract class JavaScriptStandardLibrary {
    /**
     * The file of {@code lib.es5.d.ts}: a lib directory contains it.
     */
    public static final String ES5_LIB_FILE = "lib.es5.d.ts";

    public static JavaScriptStandardLibrary getInstance() {
        return Application.get().getInstance(JavaScriptStandardLibrary.class);
    }

    /**
     * The bundled lib directory (inside the plugin archive), null when it isn't found.
     */
    @Nullable
    public abstract VirtualFile getLibDirectory();

    /**
     * A bundled lib file by library name: {@code es2015.core} is {@code lib.es2015.core.d.ts}.
     */
    @Nullable
    public abstract VirtualFile getLibFile(String name);

    /**
     * The files of the libraries of an ECMAScript level and environment libraries in a lib directory, with the
     * libraries they reference ({@code /// <reference lib="es2015.core" />}), like the {@code target} and {@code lib}
     * of tsc.
     *
     * @param level      {@code es5}, {@code es2015}, ..., {@code esnext} ({@link #getEcmaScriptLevel})
     * @param libraries  {@code dom}, {@code dom.iterable}, ...
     */
    public abstract List<VirtualFile> collectFiles(VirtualFile libDirectory, String level, Set<String> libraries);

    /**
     * The ECMAScript library level of a language version, null for a language without the standard library
     * (ActionScript).
     */
    @Nullable
    public abstract String getEcmaScriptLevel(LanguageVersion languageVersion);

    /**
     * Whether a directory is a lib directory (contains {@code lib.es5.d.ts}).
     */
    public static boolean isLibDirectory(VirtualFile directory) {
        return directory.isDirectory() && directory.findChild(ES5_LIB_FILE) != null;
    }
}

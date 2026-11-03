package consulo.javascript.debugger.cdt;

import consulo.application.Application;
import consulo.execution.debug.XSourcePosition;
import consulo.execution.debug.XSourcePositionFactory;
import consulo.javascript.debugger.cdt.sourcemap.SourceMap;
import consulo.javascript.debugger.cdt.sourcemap.SourceMapLoader;
import consulo.virtualFileSystem.LocalFileSystem;
import consulo.virtualFileSystem.VirtualFile;
import org.jspecify.annotations.Nullable;

import java.net.URI;
import java.nio.file.Path;

/**
 * Maps between positions of generated scripts and of their original sources, through
 * the scripts' source maps.
 *
 * @author VISTALL
 * @since 2026-11-01
 */
public final class SourceMappedPositions {
    private SourceMappedPositions() {
    }

    /** The original position of a generated script position, or null when the script maps none there. */
    public static @Nullable XSourcePosition original(CDTScript script, int line, int column) {
        if (!script.hasSourceMap()) {
            return null;
        }
        SourceMap sourceMap = script.getSourceMap();
        if (sourceMap == null) {
            return null;
        }
        SourceMap.Mapping mapping = sourceMap.findOriginal(line, column);
        if (mapping == null) {
            return null;
        }
        VirtualFile file = toLocalFile(sourceMap.getSources().get(mapping.sourceIndex()));
        if (file == null) {
            return null;
        }
        return Application.get().getInstance(XSourcePositionFactory.class).createPosition(file, mapping.sourceLine(), mapping.sourceColumn());
    }

    /** The index of {@code file} among the map's sources, or -1. */
    public static int sourceIndex(SourceMap sourceMap, Path file) {
        for (int i = 0; i < sourceMap.getSources().size(); i++) {
            Path source = toLocalPath(sourceMap.getSources().get(i));
            if (source != null && source.equals(file)) {
                return i;
            }
        }
        return -1;
    }

    /** A source URL (or plain path) as a normalized local path; null for a non-local source. */
    public static @Nullable Path toLocalPath(@Nullable String url) {
        URI uri = SourceMapLoader.toUri(url);
        if (uri == null) {
            return null;
        }
        try {
            if (uri.getScheme() == null) {
                return Path.of(uri.getPath()).toAbsolutePath().normalize();
            }
            if ("file".equalsIgnoreCase(uri.getScheme())) {
                return Path.of(uri).normalize();
            }
        }
        catch (Exception ignored) {
        }
        return null;
    }

    private static @Nullable VirtualFile toLocalFile(String url) {
        Path path = toLocalPath(url);
        return path == null ? null : LocalFileSystem.getInstance().findFileByNioFile(path);
    }
}

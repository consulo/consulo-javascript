package consulo.javascript.debugger.cdt.sourcemap;

import consulo.logging.Logger;
import org.jspecify.annotations.Nullable;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URLConnection;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;
import java.util.Locale;

/**
 * Loads the source map a script declares ({@code sourceMappingURL}): inline as a
 * {@code data:} URL, or a file or http(s) URL relative to the script.
 *
 * @author VISTALL
 * @since 2026-11-01
 */
public final class SourceMapLoader {
    private static final Logger LOG = Logger.getInstance(SourceMapLoader.class);

    private static final int HTTP_TIMEOUT_MILLIS = 5000;

    private SourceMapLoader() {
    }

    /** The script's source map, or null when it declares none or it cannot be read. */
    public static @Nullable SourceMap load(@Nullable String scriptUrl, @Nullable String sourceMapUrl) {
        if (sourceMapUrl == null || sourceMapUrl.isBlank()) {
            return null;
        }
        try {
            if (sourceMapUrl.regionMatches(true, 0, "data:", 0, 5)) {
                return SourceMap.parse(decodeDataUrl(sourceMapUrl), toUri(scriptUrl));
            }
            URI scriptUri = toUri(scriptUrl);
            URI mapUri = scriptUri != null ? scriptUri.resolve(sourceMapUrl) : URI.create(sourceMapUrl);
            String text = read(mapUri);
            return text == null ? null : SourceMap.parse(text, mapUri);
        }
        catch (Exception e) {
            LOG.info("Cannot load source map " + sourceMapUrl + " of " + scriptUrl + ": " + e);
            return null;
        }
    }

    /** The script URL as a URI; a plain file path (as node reports CommonJS scripts) becomes a file URI. */
    public static @Nullable URI toUri(@Nullable String url) {
        if (url == null || url.isBlank()) {
            return null;
        }
        if (url.startsWith("/") || (url.length() > 2 && url.charAt(1) == ':' && Character.isLetter(url.charAt(0)))) {
            return Path.of(url).toUri();
        }
        try {
            return URI.create(url);
        }
        catch (IllegalArgumentException e) {
            return null;
        }
    }

    private static String decodeDataUrl(String dataUrl) {
        int comma = dataUrl.indexOf(',');
        if (comma < 0) {
            throw new IllegalArgumentException("Malformed data URL");
        }
        String meta = dataUrl.substring(5, comma).toLowerCase(Locale.ROOT);
        String payload = dataUrl.substring(comma + 1);
        if (meta.endsWith(";base64")) {
            return new String(Base64.getDecoder().decode(payload), StandardCharsets.UTF_8);
        }
        return URLDecoder.decode(payload, StandardCharsets.UTF_8);
    }

    private static @Nullable String read(URI uri) throws IOException {
        String scheme = uri.getScheme();
        if (scheme == null || "file".equalsIgnoreCase(scheme)) {
            Path path = scheme == null ? Path.of(uri.getPath()) : Path.of(uri);
            return Files.isRegularFile(path) ? Files.readString(path) : null;
        }
        if ("http".equalsIgnoreCase(scheme) || "https".equalsIgnoreCase(scheme)) {
            URLConnection connection = uri.toURL().openConnection();
            connection.setConnectTimeout(HTTP_TIMEOUT_MILLIS);
            connection.setReadTimeout(HTTP_TIMEOUT_MILLIS);
            try (InputStream stream = connection.getInputStream()) {
                return new String(stream.readAllBytes(), StandardCharsets.UTF_8);
            }
        }
        return null;
    }
}

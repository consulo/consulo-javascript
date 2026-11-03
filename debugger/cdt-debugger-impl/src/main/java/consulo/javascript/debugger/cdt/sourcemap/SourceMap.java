package consulo.javascript.debugger.cdt.sourcemap;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.jspecify.annotations.Nullable;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * A parsed source map (revision 3): the mappings between positions of a generated
 * script and positions of its original sources. Lines and columns are zero-based,
 * as in the Chrome DevTools protocol.
 *
 * @author VISTALL
 * @since 2026-11-01
 */
public final class SourceMap {
    /**
     * One mapping segment: a generated position and, unless the segment maps to no source,
     * the original source index and position.
     */
    public record Mapping(int generatedLine, int generatedColumn, int sourceIndex, int sourceLine, int sourceColumn) {
        public boolean hasSource() {
            return sourceIndex >= 0;
        }
    }

    private static final String BASE64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    private static final int VLQ_BASE_SHIFT = 5;
    private static final int VLQ_BASE_MASK = (1 << VLQ_BASE_SHIFT) - 1;
    private static final int VLQ_CONTINUATION_BIT = 1 << VLQ_BASE_SHIFT;

    private final List<String> mySources;
    // mappings per generated line, ordered by generated column
    private final List<List<Mapping>> myLines;

    private SourceMap(List<String> sources, List<List<Mapping>> lines) {
        mySources = sources;
        myLines = lines;
    }

    /**
     * Parses the map text. {@code mapUrl} is where the map was loaded from: relative
     * sources resolve against it (after the map's {@code sourceRoot}).
     */
    public static SourceMap parse(String text, @Nullable URI mapUrl) {
        JsonObject root = JsonParser.parseString(text).getAsJsonObject();
        String sourceRoot = root.has("sourceRoot") && !root.get("sourceRoot").isJsonNull() ? root.get("sourceRoot").getAsString() : "";

        List<String> sources = new ArrayList<>();
        JsonArray sourceArray = root.has("sources") ? root.getAsJsonArray("sources") : new JsonArray();
        for (JsonElement element : sourceArray) {
            String source = element.isJsonNull() ? "" : element.getAsString();
            sources.add(resolveSource(sourceRoot, source, mapUrl));
        }

        String mappings = root.has("mappings") ? root.get("mappings").getAsString() : "";
        return new SourceMap(Collections.unmodifiableList(sources), decode(mappings));
    }

    /** The original sources, resolved to absolute URLs where the map's location allows it. */
    public List<String> getSources() {
        return mySources;
    }

    /** The original position of a generated position, or null when it maps to no source. */
    public @Nullable Mapping findOriginal(int generatedLine, int generatedColumn) {
        if (generatedLine < 0 || generatedLine >= myLines.size()) {
            return null;
        }
        List<Mapping> line = myLines.get(generatedLine);
        Mapping found = null;
        for (Mapping mapping : line) {
            if (mapping.generatedColumn() > generatedColumn) {
                break;
            }
            found = mapping;
        }
        if (found == null && !line.isEmpty()) {
            // a position before the first segment: the line's first mapping is the closest answer
            found = line.get(0);
        }
        return found != null && found.hasSource() ? found : null;
    }

    /**
     * The earliest generated position of an original line, or, when that line has no code,
     * of the next original line that has some within {@code maxLinesAhead}. Null when none.
     */
    public @Nullable Mapping findGenerated(int sourceIndex, int sourceLine, int maxLinesAhead) {
        Mapping best = null;
        for (List<Mapping> line : myLines) {
            for (Mapping mapping : line) {
                if (mapping.sourceIndex() != sourceIndex) {
                    continue;
                }
                int distance = mapping.sourceLine() - sourceLine;
                if (distance < 0 || distance > maxLinesAhead) {
                    continue;
                }
                if (best == null || isBetter(mapping, best)) {
                    best = mapping;
                }
            }
        }
        return best;
    }

    private static boolean isBetter(Mapping candidate, Mapping current) {
        if (candidate.sourceLine() != current.sourceLine()) {
            return candidate.sourceLine() < current.sourceLine();
        }
        if (candidate.generatedLine() != current.generatedLine()) {
            return candidate.generatedLine() < current.generatedLine();
        }
        return candidate.generatedColumn() < current.generatedColumn();
    }

    private static String resolveSource(String sourceRoot, String source, @Nullable URI mapUrl) {
        String path = sourceRoot.isEmpty() || isAbsoluteUrl(source)
            ? source
            : (sourceRoot.endsWith("/") ? sourceRoot : sourceRoot + "/") + source;
        if (isAbsoluteUrl(path) || mapUrl == null) {
            return path;
        }
        try {
            return mapUrl.resolve(new URI(null, null, path, null)).toString();
        }
        catch (Exception e) {
            return path;
        }
    }

    private static boolean isAbsoluteUrl(String value) {
        int colon = value.indexOf(':');
        // a scheme of two characters or more - "C:" is a Windows drive, not a scheme
        return colon > 1 && value.substring(0, colon).chars().allMatch(c -> Character.isLetterOrDigit(c) || c == '+' || c == '-' || c == '.');
    }

    private static List<List<Mapping>> decode(String mappings) {
        List<List<Mapping>> lines = new ArrayList<>();
        int sourceIndex = 0;
        int sourceLine = 0;
        int sourceColumn = 0;

        int generatedLine = 0;
        List<Mapping> current = new ArrayList<>();
        int position = 0;
        int length = mappings.length();
        int generatedColumn = 0;
        int[] fields = new int[5];
        while (position <= length) {
            if (position == length || mappings.charAt(position) == ';') {
                lines.add(current);
                current = new ArrayList<>();
                generatedLine++;
                generatedColumn = 0;
                position++;
                continue;
            }
            if (mappings.charAt(position) == ',') {
                position++;
                continue;
            }

            int count = 0;
            while (position < length && mappings.charAt(position) != ',' && mappings.charAt(position) != ';') {
                int value = 0;
                int shift = 0;
                int digit;
                do {
                    int index = BASE64.indexOf(mappings.charAt(position++));
                    if (index < 0) {
                        throw new IllegalArgumentException("Invalid base64 VLQ character in source map mappings");
                    }
                    digit = index;
                    value += (digit & VLQ_BASE_MASK) << shift;
                    shift += VLQ_BASE_SHIFT;
                }
                while ((digit & VLQ_CONTINUATION_BIT) != 0);
                int decoded = (value & 1) == 1 ? -(value >>> 1) : (value >>> 1);
                if (count < fields.length) {
                    fields[count] = decoded;
                }
                count++;
            }

            generatedColumn += fields[0];
            if (count >= 4) {
                sourceIndex += fields[1];
                sourceLine += fields[2];
                sourceColumn += fields[3];
                current.add(new Mapping(generatedLine, generatedColumn, sourceIndex, sourceLine, sourceColumn));
            }
            else {
                current.add(new Mapping(generatedLine, generatedColumn, -1, -1, -1));
            }
        }
        for (List<Mapping> line : lines) {
            line.sort((a, b) -> Integer.compare(a.generatedColumn(), b.generatedColumn()));
        }
        return lines;
    }
}

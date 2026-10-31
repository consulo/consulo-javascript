package consulo.typescript.library;

import consulo.annotation.component.ServiceImpl;
import consulo.javascript.language.JavaScriptLanguageVersion;
import consulo.javascript.language.StandardJavaScriptVersions;
import consulo.javascript.library.JavaScriptStandardLibrary;
import consulo.language.version.LanguageVersion;
import consulo.logging.Logger;
import consulo.typescript.version.BaseTypeScriptLanguageVersion;
import consulo.virtualFileSystem.StandardFileSystems;
import consulo.virtualFileSystem.VirtualFile;
import consulo.virtualFileSystem.VirtualFileManager;
import consulo.virtualFileSystem.util.VirtualFileUtil;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;
import org.jspecify.annotations.Nullable;

import java.io.IOException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The standard library declarations of TypeScript (lib.es5.d.ts, lib.es2015.*.d.ts, lib.dom.d.ts, ..., from the
 * {@code typescript} npm package, Apache-2.0), bundled in {@code typescript-lib/}: the roots of the Web SDK and of the
 * Node.js SDKs. Which of them a module sees follows its JavaScript extensions, like the {@code target} and {@code lib}
 * of tsc.
 *
 * @since 2026-10-31
 */
@Singleton
@ServiceImpl
public class TypeScriptStandardLibraryImpl extends JavaScriptStandardLibrary {
    private static final Logger LOG = Logger.getInstance(TypeScriptStandardLibraryImpl.class);

    // not a valid package name: the resources aren't encapsulated by the module
    private static final String LIB_DIRECTORY = "typescript-lib";
    private static final String LIB_PREFIX = "lib.";
    private static final String LIB_SUFFIX = ".d.ts";

    private static final Pattern REFERENCE_LIB = Pattern.compile("^/// *<reference +lib=\"([^\"]+)\"", Pattern.MULTILINE);

    // the references are at the top of the files, after the license header
    private static final int HEADER_LENGTH = 8192;

    private final VirtualFileManager myVirtualFileManager;

    private volatile @Nullable VirtualFile myLibDirectory;

    // by directory, level and libraries: the files of a lib directory don't change
    private final ConcurrentMap<String, List<VirtualFile>> myFilesByLibraries = new ConcurrentHashMap<>();

    @Inject
    public TypeScriptStandardLibraryImpl(VirtualFileManager virtualFileManager) {
        myVirtualFileManager = virtualFileManager;
    }

    @Nullable
    @Override
    public VirtualFile getLibDirectory() {
        VirtualFile directory = myLibDirectory;
        if (directory != null && directory.isValid()) {
            return directory;
        }
        directory = findLibDirectory();
        myLibDirectory = directory;
        return directory;
    }

    @Nullable
    @Override
    public VirtualFile getLibFile(String name) {
        VirtualFile directory = getLibDirectory();
        return directory == null ? null : directory.findChild(LIB_PREFIX + name + LIB_SUFFIX);
    }

    @Override
    public List<VirtualFile> collectFiles(VirtualFile libDirectory, String level, Set<String> libraries) {
        String key = libDirectory.getUrl() + "|" + level + "|" + new TreeSet<>(libraries);
        List<VirtualFile> files = myFilesByLibraries.computeIfAbsent(key, it -> collect(libDirectory, level, libraries));
        if (files.stream().allMatch(VirtualFile::isValid)) {
            return files;
        }
        files = collect(libDirectory, level, libraries);
        myFilesByLibraries.put(key, files);
        return files;
    }

    private static List<VirtualFile> collect(VirtualFile libDirectory, String level, Set<String> libraries) {
        Deque<String> queue = new ArrayDeque<>();
        queue.add(libDirectory.findChild(LIB_PREFIX + level + LIB_SUFFIX) != null ? level : "esnext");
        queue.addAll(libraries);

        Set<String> visited = new LinkedHashSet<>();
        List<VirtualFile> result = new ArrayList<>();
        while (!queue.isEmpty()) {
            String name = queue.poll();
            VirtualFile file = libDirectory.findChild(LIB_PREFIX + name + LIB_SUFFIX);
            if (file == null || !visited.add(name)) {
                continue;
            }
            result.add(file);
            queue.addAll(readReferencedLibs(file));
        }
        return List.copyOf(result);
    }

    /**
     * es5, es2015, ..., esnext: ECMAScript N is ECMAScript (2009 + N) from 6 on, TypeScript is esnext. Null for the
     * versions with their own SDK (ActionScript, ECMAScript 4).
     */
    @Nullable
    @Override
    public String getEcmaScriptLevel(LanguageVersion version) {
        if (version instanceof BaseTypeScriptLanguageVersion) {
            return "esnext";
        }
        if (!(version instanceof JavaScriptLanguageVersion)) {
            return null;
        }
        String id = version.getId();
        if (id.startsWith("ECMASCRIPT_")) {
            try {
                int edition = Integer.parseInt(id.substring("ECMASCRIPT_".length()));
                return edition >= 6 ? "es" + (2009 + edition) : "es5";
            }
            catch (NumberFormatException e) {
                return "esnext";
            }
        }
        if (id.startsWith("JAVASCRIPT_")) {
            return "es5";
        }
        if (id.equals("JSX")) {
            JavaScriptLanguageVersion defaultVersion = StandardJavaScriptVersions.getInstance().getDefaultVersion();
            return defaultVersion.getId().equals("JSX") ? "esnext" : getEcmaScriptLevel(defaultVersion);
        }
        return null;
    }

    private static List<String> readReferencedLibs(VirtualFile file) {
        try {
            byte[] bytes = file.contentsToByteArray();
            String header = new String(bytes, 0, Math.min(bytes.length, HEADER_LENGTH), StandardCharsets.UTF_8);
            List<String> result = new ArrayList<>();
            Matcher matcher = REFERENCE_LIB.matcher(header);
            while (matcher.find()) {
                result.add(matcher.group(1).toLowerCase());
            }
            return result;
        }
        catch (IOException e) {
            LOG.warn(e);
            return List.of();
        }
    }

    @Nullable
    private VirtualFile findLibDirectory() {
        URL resource = TypeScriptStandardLibraryImpl.class.getClassLoader().getResource(LIB_DIRECTORY + "/" + ES5_LIB_FILE);
        if (resource == null) {
            LOG.error("Bundled TypeScript libraries not found: " + LIB_DIRECTORY);
            return null;
        }
        String url = VirtualFileUtil.convertFromUrl(resource);
        // the archives of the plugins are in the zip file system
        if (url.startsWith(StandardFileSystems.JAR_PROTOCOL_PREFIX)) {
            url = StandardFileSystems.ZIP_PROTOCOL_PREFIX + url.substring(StandardFileSystems.JAR_PROTOCOL_PREFIX.length());
        }
        VirtualFile file = myVirtualFileManager.findFileByUrl(url);
        if (file == null) {
            file = myVirtualFileManager.refreshAndFindFileByUrl(url);
        }
        return file != null ? file.getParent() : null;
    }
}

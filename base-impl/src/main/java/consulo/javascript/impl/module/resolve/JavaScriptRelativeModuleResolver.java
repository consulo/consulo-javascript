/*
 * Copyright 2013-2026 consulo.io
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package consulo.javascript.impl.module.resolve;

import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.module.resolve.JavaScriptModuleRequest;
import consulo.javascript.module.resolve.JavaScriptModuleResolver;
import consulo.virtualFileSystem.VirtualFile;
import org.jspecify.annotations.Nullable;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A path relative to the importing file ({@code './a'}, {@code '../lib/b.js'}), like TypeScript and bundlers resolve
 * it: the file as written or with an extension, else a directory with its package.json entry or index file.
 * <p>
 * For the declarations (TypeScript) the TypeScript files come first and {@code './x.js'} also means {@code x.ts}; for
 * the code (an import, a require) the JavaScript files come first.
 *
 * @since 2026-10-31
 */
@ExtensionImpl
public class JavaScriptRelativeModuleResolver implements JavaScriptModuleResolver {
    private static final List<String> TYPES_EXTENSIONS = List.of(".ts", ".tsx", ".d.ts", ".js", ".jsx", ".mjs", ".cjs");
    private static final List<String> SCRIPT_EXTENSIONS = List.of(".js", ".mjs", ".cjs", ".jsx", ".ts", ".tsx", ".d.ts", ".json");
    private static final Pattern TYPES_FIELD = Pattern.compile("\"(?:types|typings)\"\\s*:\\s*\"([^\"]+)\"");
    private static final Pattern MAIN_FIELD = Pattern.compile("\"main\"\\s*:\\s*\"([^\"]+)\"");

    @RequiredReadAction
    @Override
    public @Nullable VirtualFile resolve(JavaScriptModuleRequest request) {
        if (!request.isRelative()) {
            return null;
        }
        return findModuleFile(request.directory(), request.specifier(), request.kind() == JavaScriptModuleRequest.Kind.TYPES);
    }

    private static @Nullable VirtualFile findModuleFile(VirtualFile directory, String path, boolean types) {
        List<String> extensions = types ? TYPES_EXTENSIONS : SCRIPT_EXTENSIONS;
        // import './x.js' of TypeScript also means x.ts
        if (types && path.endsWith(".js")) {
            String withoutJs = path.substring(0, path.length() - 3);
            for (String extension : extensions) {
                VirtualFile file = directory.findFileByRelativePath(withoutJs + extension);
                if (file != null && !file.isDirectory()) {
                    return file;
                }
            }
        }
        VirtualFile exact = directory.findFileByRelativePath(path);
        if (exact != null && !exact.isDirectory()) {
            return exact;
        }
        for (String extension : extensions) {
            VirtualFile file = directory.findFileByRelativePath(path + extension);
            if (file != null && !file.isDirectory()) {
                return file;
            }
        }
        return exact != null ? findDirectoryEntry(exact, types) : null;
    }

    /**
     * The entry of a directory: the {@code types} (declarations) or {@code main} of its package.json, else its index
     */
    private static @Nullable VirtualFile findDirectoryEntry(VirtualFile directory, boolean types) {
        VirtualFile packageJson = directory.findChild("package.json");
        if (packageJson != null && !packageJson.isDirectory()) {
            try {
                String text = new String(packageJson.contentsToByteArray(), StandardCharsets.UTF_8);
                Matcher matcher = (types ? TYPES_FIELD : MAIN_FIELD).matcher(text);
                if (matcher.find()) {
                    VirtualFile entry = directory.findFileByRelativePath(matcher.group(1));
                    if (entry != null && !entry.isDirectory()) {
                        return entry;
                    }
                }
            }
            catch (IOException ignored) {
            }
        }
        for (String extension : types ? TYPES_EXTENSIONS : SCRIPT_EXTENSIONS) {
            VirtualFile index = directory.findChild("index" + extension);
            if (index != null && !index.isDirectory()) {
                return index;
            }
        }
        return null;
    }
}

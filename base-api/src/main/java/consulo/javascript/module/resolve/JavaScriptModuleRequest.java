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
package consulo.javascript.module.resolve;

import consulo.language.psi.PsiFile;
import consulo.virtualFileSystem.VirtualFile;

/**
 * A module specifier to resolve: {@code import ... from 'vue'}, {@code require('./util')}.
 *
 * @param file      the importing file (the original file of a copy, like of the completion)
 * @param directory the directory of the importing file
 * @param specifier the specifier as written
 * @param kind      what is imported: the source for an {@code import}, a {@code require}, or the declarations for
 *                  TypeScript
 * @since 2026-10-31
 */
public record JavaScriptModuleRequest(PsiFile file, VirtualFile directory, String specifier, Kind kind) {
    public enum Kind {
        /**
         * An ES module import ({@code import}, {@code export ... from}, {@code import()})
         */
        IMPORT,
        /**
         * A CommonJS {@code require}
         */
        REQUIRE,
        /**
         * The declarations of a module, for TypeScript: {@code types} of the package, {@code .d.ts}, {@code @types}
         */
        TYPES
    }

    /**
     * @return whether the specifier is a path ({@code ./x}, {@code ../x}, {@code /x}), not a package name
     */
    public boolean isRelative() {
        return specifier.startsWith(".") || specifier.startsWith("/");
    }
}

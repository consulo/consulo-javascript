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

import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ComponentScope;
import consulo.annotation.component.ExtensionAPI;
import consulo.virtualFileSystem.VirtualFile;
import org.jspecify.annotations.Nullable;

/**
 * Resolves module specifiers to files, like a runtime or a bundler does: the packages of npm ({@code node_modules},
 * the {@code exports} of a package.json, the workspaces of a monorepo), path aliases of a build tool, ...
 * <p>
 * The resolvers are asked in order ({@link JavaScriptModuleResolution}); the first file wins. The callers keep their
 * own resolution as the fallback.
 *
 * @since 2026-10-31
 */
@ExtensionAPI(ComponentScope.APPLICATION)
public interface JavaScriptModuleResolver {
    /**
     * @return the file of the module, {@code null} if this resolver doesn't know it
     */
    @RequiredReadAction
    @Nullable
    VirtualFile resolve(JavaScriptModuleRequest request);
}

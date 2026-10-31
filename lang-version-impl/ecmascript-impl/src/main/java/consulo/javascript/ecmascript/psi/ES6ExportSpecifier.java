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
package consulo.javascript.ecmascript.psi;

import com.intellij.lang.javascript.psi.JSElement;
import consulo.annotation.access.RequiredReadAction;
import org.jspecify.annotations.Nullable;

/**
 * {@code a} or {@code a as b} of {@code export { ... }}.
 *
 * @since 2026-10-31
 */
public interface ES6ExportSpecifier extends JSElement {
    /**
     * @return {@code a} of {@code a as b}: the local declaration, or the export of the {@code from} module
     */
    @RequiredReadAction
    @Nullable
    String getLocalName();

    /**
     * @return {@code b} of {@code a as b}, else {@code a}: the name the module exports
     */
    @RequiredReadAction
    @Nullable
    String getExportedName();
}

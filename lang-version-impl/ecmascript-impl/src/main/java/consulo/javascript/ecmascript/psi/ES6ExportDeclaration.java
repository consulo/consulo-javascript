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

import com.intellij.lang.javascript.psi.JSStatement;
import consulo.annotation.access.RequiredReadAction;
import org.jspecify.annotations.Nullable;

import java.util.List;

/**
 * {@code export { a, b as c };}, {@code export { a } from './x';}, {@code export * from './x';},
 * {@code export * as ns from './x';}
 *
 * @since 2026-10-31
 */
public interface ES6ExportDeclaration extends JSStatement {
    /**
     * @return whether it is {@code export * from} (or {@code export * as ns from})
     */
    @RequiredReadAction
    boolean isExportAll();

    /**
     * @return {@code ns} of {@code export * as ns from}
     */
    @RequiredReadAction
    @Nullable
    String getNamespaceName();

    @RequiredReadAction
    List<ES6ExportSpecifier> getSpecifiers();

    /**
     * @return the module of {@code from}, unquoted; {@code null} for the exports of local declarations
     */
    @RequiredReadAction
    @Nullable
    String getModuleSpecifier();
}

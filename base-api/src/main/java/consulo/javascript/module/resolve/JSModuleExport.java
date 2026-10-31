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

import consulo.index.io.StringRef;
import consulo.language.psi.stub.StubInputStream;
import consulo.language.psi.stub.StubOutputStream;
import org.jspecify.annotations.Nullable;

import java.io.IOException;

/**
 * An export of an ES module, kept in the stub of its file: the exports of a module are found without its tree.
 *
 * @param name      the exported name ({@link JavaScriptModuleExports#DEFAULT_EXPORT} for {@code export default})
 * @param localName the local declaration, or the name exported by the module of {@code specifier}
 * @param specifier the module of a re-export
 * @since 2026-10-31
 */
public record JSModuleExport(Kind kind, @Nullable String name, @Nullable String localName, @Nullable String specifier) {
    /**
     * The local name of {@code export * as ns from './x'}: the module itself.
     */
    public static final String NAMESPACE_EXPORT = "*";

    public enum Kind {
        /**
         * {@code export class A}, {@code export {a as b}}, {@code export default a}.
         */
        LOCAL,
        /**
         * {@code export {a as b} from './x'}, {@code export * as ns from './x'}, and an imported name exported again.
         */
        REEXPORT,
        /**
         * {@code export * from './x'}.
         */
        STAR,
        /**
         * {@code export = X} (CommonJS declaration files): X is the default export, its namespace members the named ones.
         */
        EQUALS
    }

    public static JSModuleExport local(String name, @Nullable String localName) {
        return new JSModuleExport(Kind.LOCAL, name, localName, null);
    }

    public static JSModuleExport reexport(String name, String localName, String specifier) {
        return new JSModuleExport(Kind.REEXPORT, name, localName, specifier);
    }

    public static JSModuleExport star(String specifier) {
        return new JSModuleExport(Kind.STAR, null, null, specifier);
    }

    public static JSModuleExport equals(String localName) {
        return new JSModuleExport(Kind.EQUALS, null, localName, null);
    }

    public void serialize(StubOutputStream dataStream) throws IOException {
        dataStream.writeVarInt(kind.ordinal());
        dataStream.writeName(name);
        dataStream.writeName(localName);
        dataStream.writeName(specifier);
    }

    public static JSModuleExport deserialize(StubInputStream dataStream) throws IOException {
        Kind kind = Kind.values()[dataStream.readVarInt()];
        String name = StringRef.toString(dataStream.readName());
        String localName = StringRef.toString(dataStream.readName());
        String specifier = StringRef.toString(dataStream.readName());
        return new JSModuleExport(kind, name, localName, specifier);
    }
}

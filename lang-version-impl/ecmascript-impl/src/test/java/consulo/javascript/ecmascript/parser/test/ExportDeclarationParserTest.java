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
package consulo.javascript.ecmascript.parser.test;

import consulo.javascript.ecmascript.lang.EcmaScript12JavaScriptVersion;
import consulo.javascript.language.JavaScriptFileType;
import consulo.language.file.LanguageFileType;
import consulo.language.version.LanguageVersion;
import consulo.test.junit.impl.language.SimpleParsingTest;
import consulo.virtualFileSystem.fileType.FileType;
import org.jspecify.annotations.Nullable;
import org.junit.jupiter.api.Test;

/**
 * The export lists and re-exports of ES modules: {@code export { a as b }}, {@code export { a } from},
 * {@code export * from}, {@code export * as ns from}.
 */
public class ExportDeclarationParserTest extends SimpleParsingTest<Object> {
    public ExportDeclarationParserTest() {
        super("exportDeclarations", "js");
    }

    @Test
    public void testExportLists(Context context) throws Exception {
        doTest(context, null);
    }

    @Override
    protected LanguageFileType getFileType(Context context, @Nullable Object o) {
        return JavaScriptFileType.INSTANCE;
    }

    @Override
    protected LanguageVersion resolveLanguageVersion(Context context, @Nullable Object o, FileType fileType) {
        return EcmaScript12JavaScriptVersion.getInstance();
    }
}

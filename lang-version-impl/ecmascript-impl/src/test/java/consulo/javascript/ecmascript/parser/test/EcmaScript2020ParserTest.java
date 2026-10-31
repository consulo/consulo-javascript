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
 * ES2020: nullish coalescing.
 *
 * @since 2026-10-31
 */
public class EcmaScript2020ParserTest extends SimpleParsingTest<Object> {
    public EcmaScript2020ParserTest() {
        super("es2020", "js");
    }

    @Test
    public void testNullishCoalescing(Context context) throws Exception {
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

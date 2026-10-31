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
 * async / await (ES2017), issues #38 / #39.
 *
 * @since 2026-10-31
 */
public class AsyncAwaitParserTest extends SimpleParsingTest<Object> {
    public AsyncAwaitParserTest() {
        super("asyncAwait", "js");
    }

    @Test
    public void testAsyncFunction(Context context) throws Exception {
        doTest(context, null);
    }

    @Test
    public void testAsyncArrow(Context context) throws Exception {
        doTest(context, null);
    }

    @Test
    public void testAsyncMethods(Context context) throws Exception {
        doTest(context, null);
    }

    @Test
    public void testForAwait(Context context) throws Exception {
        doTest(context, null);
    }

    @Test
    public void testAsyncAwaitAsIdentifiers(Context context) throws Exception {
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

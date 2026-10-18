package consulo.javascript.ecmascript.parser.test;

import consulo.javascript.ecmascript.lang.EcmaScript6JavaScriptVersion;
import consulo.javascript.language.JavaScriptFileType;
import consulo.language.file.LanguageFileType;
import consulo.language.version.LanguageVersion;
import consulo.test.junit.impl.language.SimpleParsingTest;
import consulo.virtualFileSystem.fileType.FileType;
import org.junit.jupiter.api.Test;

/**
 * @author VISTALL
 * @since 2026-10-18
 */
public class EcmaScriptParserTest extends SimpleParsingTest<Object> {
    public EcmaScriptParserTest() {
        super("parsing", "js");
    }

    @Test
    public void testTemplateLiterals(Context context) throws Exception {
        doTest(context, null);
    }

    @Test
    public void testTaggedTemplates(Context context) throws Exception {
        doTest(context, null);
    }

    @Test
    public void testUnclosedTemplate(Context context) throws Exception {
        doTest(context, null);
    }

    @Test
    public void testUnclosedSubstitution(Context context) throws Exception {
        doTest(context, null);
    }

    @Override
    protected LanguageFileType getFileType(Context context, Object o) {
        return JavaScriptFileType.INSTANCE;
    }

    @Override
    protected LanguageVersion resolveLanguageVersion(Context context, Object o, FileType fileType) {
        return EcmaScript6JavaScriptVersion.getInstance();
    }
}

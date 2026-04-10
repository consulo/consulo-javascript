package consulo.typescript.parser.test;

import consulo.language.file.LanguageFileType;
import consulo.test.junit.impl.language.SimpleParsingTest;
import consulo.typescript.TypeScriptFileType;
import org.junit.jupiter.api.Test;

/**
 * @author VISTALL
 * @since 2026-04-06
 */
public class TypeScriptParserTest extends SimpleParsingTest<Object> {
    public TypeScriptParserTest() {
        super("parsing", "ts");
    }

    @Test
    public void testSimple(Context context) throws Exception {
        doTest(context, null);
    }

    @Override
    protected LanguageFileType getFileType(Context context, Object o) {
        return TypeScriptFileType.INSTANCE;
    }
}

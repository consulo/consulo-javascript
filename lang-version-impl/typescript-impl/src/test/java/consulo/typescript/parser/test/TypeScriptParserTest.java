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

    @Test
    public void testVarDeclaration(Context context) throws Exception {
        doTest(context, null);
    }

    @Test
    public void testFunctionDeclaration(Context context) throws Exception {
        doTest(context, null);
    }

    @Test
    public void testClassDeclaration(Context context) throws Exception {
        doTest(context, null);
    }

    @Test
    public void testInterfaceDeclaration(Context context) throws Exception {
        doTest(context, null);
    }

    @Test
    public void testEnumDeclaration(Context context) throws Exception {
        doTest(context, null);
    }

    @Test
    public void testTypeAlias(Context context) throws Exception {
        doTest(context, null);
    }

    @Test
    public void testImportExport(Context context) throws Exception {
        doTest(context, null);
    }

    @Test
    public void testImportMeta(Context context) throws Exception {
        doTest(context, null);
    }

    @Test
    public void testAmbientDeclaration(Context context) throws Exception {
        doTest(context, null);
    }

    @Test
    public void testArrowFunction(Context context) throws Exception {
        doTest(context, null);
    }

    @Test
    public void testGenericTypes(Context context) throws Exception {
        doTest(context, null);
    }

    @Test
    public void testStringLiterals(Context context) throws Exception {
        doTest(context, null);
    }

    @Test
    public void testIfElse(Context context) throws Exception {
        doTest(context, null);
    }

    @Test
    public void testObjectLiteralMethod(Context context) throws Exception {
        doTest(context, null);
    }

    @Test
    public void testExportType(Context context) throws Exception {
        doTest(context, null);
    }

    @Test
    public void testDeclareModule(Context context) throws Exception {
        doTest(context, null);
    }

    @Override
    protected LanguageFileType getFileType(Context context, Object o) {
        return TypeScriptFileType.INSTANCE;
    }
}

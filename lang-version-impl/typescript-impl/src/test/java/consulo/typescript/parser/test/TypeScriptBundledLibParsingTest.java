package consulo.typescript.parser.test;

import consulo.language.file.LanguageFileType;
import consulo.language.psi.PsiErrorElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.test.junit.impl.language.SimpleParsingTest;
import consulo.typescript.TypeScriptFileType;
import org.junit.jupiter.api.Test;

import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The bundled standard library declarations (typescript-lib/lib.*.d.ts) parse without errors: a declaration after a
 * parse error can be lost for the stub index.
 */
public class TypeScriptBundledLibParsingTest extends SimpleParsingTest<Object> {
    public TypeScriptBundledLibParsingTest() {
        super("parsing", "ts");
    }

    @Test
    public void testNoParseErrors(Context context) throws Exception {
        URL license = getClass().getClassLoader().getResource("typescript-lib/LICENSE.txt");
        assertNotNull(license);
        Path dir = Path.of(license.toURI()).getParent();
        List<Path> libs;
        try (Stream<Path> files = Files.list(dir)) {
            libs = files.filter(path -> path.getFileName().toString().endsWith(".d.ts")).sorted().toList();
        }
        assertTrue(libs.size() > 50, libs.toString());

        StringBuilder errors = new StringBuilder();
        for (Path lib : libs) {
            String text = Files.readString(lib);
            PsiFile file = createFile(context, null, lib.getFileName().toString(), TypeScriptFileType.INSTANCE, text);
            for (PsiErrorElement error : PsiTreeUtil.findChildrenOfType(file, PsiErrorElement.class)) {
                int offset = error.getTextRange().getStartOffset();
                int lineStart = text.lastIndexOf('\n', offset - 1) + 1;
                int lineEnd = text.indexOf('\n', offset);
                errors.append(lib.getFileName()).append(':').append(text.substring(0, offset).split("\n", -1).length)
                    .append(": ").append(error.getErrorDescription()).append("\n    ")
                    .append(text, lineStart, lineEnd < 0 ? text.length() : lineEnd).append('\n');
            }
        }
        assertEquals("", errors.toString());
    }

    @Override
    protected LanguageFileType getFileType(Context context, Object o) {
        return TypeScriptFileType.INSTANCE;
    }
}

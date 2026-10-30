package consulo.typescript.api.test;

import com.intellij.lang.javascript.patterns.JSPatterns;
import com.intellij.lang.javascript.psi.JSCallExpression;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import com.intellij.lang.javascript.psi.JSSpreadExpression;
import com.intellij.lang.javascript.psi.ecmal4.JSReferenceListMember;
import consulo.javascript.ecmascript.psi.ES6ExportDefaultAssignment;
import consulo.javascript.ecmascript.psi.ES6FromClause;
import consulo.javascript.ecmascript.psi.ES6ImportDeclaration;
import consulo.language.file.LanguageFileType;
import consulo.language.psi.PsiFile;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.test.junit.impl.language.SimpleParsingTest;
import consulo.typescript.TypeScriptFileType;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The IntelliJ-compatible JavaScript API (JSPatterns, JSLiteralExpression#getStringValue, ES6 import/export contracts,
 * reference list members, JSCallExpression#getArguments, JSFunction#getBlock) on TypeScript PSI.
 */
public class JavaScriptApiOnTypeScriptTest extends SimpleParsingTest<Object> {
    public JavaScriptApiOnTypeScriptTest() {
        super("api", "ts");
    }

    @Test
    public void testStringValue(Context context) throws Exception {
        PsiFile file = file(context, "const a = 'it\\'s'; const b = \"x\\ny\"; const c = 42;");
        List<JSLiteralExpression> literals = new ArrayList<>(PsiTreeUtil.findChildrenOfType(file, JSLiteralExpression.class));
        assertEquals("it's", literals.get(0).getStringValue());
        assertTrue(literals.get(0).isQuotedLiteral());
        assertEquals("x\ny", literals.get(1).getStringValue());
        assertNull(literals.get(2).getStringValue());
        assertFalse(literals.get(2).isQuotedLiteral());
    }

    @Test
    public void testArgumentPattern(Context context) throws Exception {
        PsiFile file = file(context, """
            Routing.generate('route_a');
            fos.Router.generate('route_b');
            new Request('url');
            other('nope');
            Routing.generate(x, 'second');
            """);
        List<String> matched = new ArrayList<>();
        for (JSLiteralExpression literal : PsiTreeUtil.findChildrenOfType(file, JSLiteralExpression.class)) {
            // light tests have no InjectedLanguageManager for inside(): the literal is the argument itself
            if (JSPatterns.jsLiteralExpression().and(JSPatterns.or(
                JSPatterns.jsArgument(JSPatterns.jsReferenceExpression().withQualifiedName("Routing.generate"), 0),
                JSPatterns.jsArgument(JSPatterns.jsReferenceExpression().withQualifiedName("fos.Router.generate"), 0),
                JSPatterns.jsArgument("Request", 0)
            )).accepts(literal)) {
                matched.add(literal.getStringValue());
            }
        }
        assertEquals(List.of("route_a", "route_b", "url"), matched);
    }

    @Test
    public void testCallArgumentsAndSpread(Context context) throws Exception {
        PsiFile file = file(context, "Encore.addEntry('app', './app.js', ...rest);");
        JSCallExpression call = PsiTreeUtil.findChildOfType(file, JSCallExpression.class);
        assertNotNull(call);
        assertEquals(3, call.getArguments().length);
        JSSpreadExpression spread = PsiTreeUtil.findChildOfType(file, JSSpreadExpression.class);
        assertNotNull(spread);
    }

    @Test
    public void testImportFromClause(Context context) throws Exception {
        PsiFile file = file(context, "import { Controller } from '@hotwired/stimulus';");
        ES6ImportDeclaration declaration = PsiTreeUtil.findChildOfType(file, ES6ImportDeclaration.class);
        assertNotNull(declaration);
        ES6FromClause fromClause = declaration.getFromClause();
        assertNotNull(fromClause);
        assertEquals("'@hotwired/stimulus'", fromClause.getReferenceText());
    }

    @Test
    public void testExportDefaultExpression(Context context) throws Exception {
        PsiFile file = file(context, "export default defineConfig({ build: {} });");
        ES6ExportDefaultAssignment export = PsiTreeUtil.findChildOfType(file, ES6ExportDefaultAssignment.class);
        assertNotNull(export);
        assertTrue(export.getExpression() instanceof JSCallExpression);
    }

    @Test
    public void testExtendsListMembers(Context context) throws Exception {
        PsiFile file = file(context, "class Hello extends Controller implements A, ns.B {\n}");
        JSClass jsClass = PsiTreeUtil.findChildOfType(file, JSClass.class);
        assertNotNull(jsClass);
        assertNotNull(jsClass.getExtendsList());
        JSReferenceListMember[] members = jsClass.getExtendsList().getMembers();
        assertEquals(List.of("Controller"), Arrays.stream(members).map(JSReferenceListMember::getReferencedName).toList());
        assertNotNull(jsClass.getImplementsList());
        assertEquals(List.of("A", "ns.B"),
            Arrays.stream(jsClass.getImplementsList().getMembers()).map(JSReferenceListMember::getReferenceText).toList());
    }

    @Test
    public void testFunctionBlockAndParameters(Context context) throws Exception {
        PsiFile file = file(context, "export default defineConfig(({ mode }, extra) => { return {}; });\nconst f = (x) => x * 2;");
        List<JSFunction> functions = new ArrayList<>(PsiTreeUtil.findChildrenOfType(file, JSFunction.class));
        assertEquals(2, functions.get(0).getParameterVariables().length);
        assertNotNull(functions.get(0).getBlock());
        assertEquals(1, functions.get(1).getParameterVariables().length);
        assertNull(functions.get(1).getBlock());
    }

    private PsiFile file(Context context, String text) {
        return createFile(context, null, "a.ts", TypeScriptFileType.INSTANCE, text);
    }

    @Override
    protected LanguageFileType getFileType(Context context, Object o) {
        return TypeScriptFileType.INSTANCE;
    }
}

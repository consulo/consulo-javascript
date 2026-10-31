package consulo.typescript.it;

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import consulo.application.ReadAction;
import consulo.it.CodeInsightTestFixture;
import consulo.it.HeadlessProjectExtension;
import consulo.language.editor.completion.lookup.LookupElement;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.PsiPolyVariantReference;
import consulo.language.psi.PsiReference;
import consulo.language.psi.ResolveResult;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The standard ECMAScript and browser globals resolve to the bundled TypeScript library declarations (lib.*.d.ts), in
 * JavaScript and TypeScript files, without a configured SDK (consulo-javascript#22).
 */
@ExtendWith(HeadlessProjectExtension.class)
public class PredefinedLibraryResolveTest {
    /**
     * The standard library comes with the Web extension of the module.
     */
    @BeforeEach
    public void enableWeb(CodeInsightTestFixture fixture) throws Exception {
        WebModules.enableWeb(fixture.getModule());
    }

    @Test
    public void testGlobalFunction(CodeInsightTestFixture fixture) throws Exception {
        for (String extension : List.of("js", "ts")) {
            PsiElement target = resolve(fixture, extension, "setInter<caret>val(function () {}, 1000);\n");
            assertDeclared(target, "setInterval", "lib.dom.d.ts");
            assertInstanceOf(JSFunction.class, target);
        }
    }

    @Test
    public void testDateMethod(CodeInsightTestFixture fixture) throws Exception {
        for (String extension : List.of("js", "ts")) {
            PsiElement target = resolve(fixture, extension, "var time = new Date().getT<caret>ime();\n");
            assertDeclared(target, "getTime", "lib.es5.d.ts");
            assertInstanceOf(JSFunction.class, target);
        }
    }

    @Test
    public void testDate(CodeInsightTestFixture fixture) throws Exception {
        for (String extension : List.of("js", "ts")) {
            assertDeclared(resolve(fixture, extension, "var date = new Da<caret>te();\n"), "Date", "lib.es5.d.ts");
        }
    }

    @Test
    public void testXmlHttpRequest(CodeInsightTestFixture fixture) throws Exception {
        for (String extension : List.of("js", "ts")) {
            assertDeclared(resolve(fixture, extension, "var request = new XMLHttp<caret>Request();\n"), "XMLHttpRequest", "lib.dom.d.ts");
        }
    }

    @Test
    public void testXmlHttpRequestMethod(CodeInsightTestFixture fixture) throws Exception {
        for (String extension : List.of("js", "ts")) {
            PsiElement target = resolve(fixture, extension, "new XMLHttpRequest().op<caret>en('GET', '/');\n");
            assertDeclared(target, "open", "lib.dom.d.ts");
            assertInstanceOf(JSFunction.class, target);
        }
    }

    @Test
    public void testPromise(CodeInsightTestFixture fixture) throws Exception {
        for (String extension : List.of("js", "ts")) {
            PsiElement target = resolve(fixture, extension, "var p = new Prom<caret>ise(function (resolve) { resolve(1); });\n");
            assertDeclared(target, "Promise", null);
            String file = ReadAction.compute(() -> target.getContainingFile().getName());
            assertTrue(file.startsWith("lib.es") && file.endsWith(".d.ts"), file);
        }
    }

    @Test
    public void testConsoleAndDocument(CodeInsightTestFixture fixture) throws Exception {
        for (String extension : List.of("js", "ts")) {
            assertDeclared(resolve(fixture, extension, "con<caret>sole.log(1);\n"), "console", "lib.dom.d.ts");
            assertDeclared(resolve(fixture, extension, "docu<caret>ment.title = 'a';\n"), "document", "lib.dom.d.ts");
            assertDeclared(resolve(fixture, extension, "win<caret>dow.alert('a');\n"), "window", "lib.dom.d.ts");
        }
    }

    @Test
    public void testMapAndJson(CodeInsightTestFixture fixture) throws Exception {
        for (String extension : List.of("js", "ts")) {
            assertDeclared(resolve(fixture, extension, "var m = new M<caret>ap();\n"), "Map", "lib.es2015.collection.d.ts");
            assertDeclared(resolve(fixture, extension, "JS<caret>ON.stringify({});\n"), "JSON", "lib.es5.d.ts");
        }
    }

    @Test
    public void testUnknownGlobalStaysUnresolved(CodeInsightTestFixture fixture) throws Exception {
        for (String extension : List.of("js", "ts")) {
            fixture.configureByText("app." + extension, "noSuchGlo<caret>bal();\n");
            PsiReference reference = fixture.getReferenceAtCaretPositionWithAssertion();
            assertNull(ReadAction.compute(reference::resolve));
        }
    }

    /**
     * A property of an interface ({@code document.title}), the constructed type of an inline construct signature
     * ({@code declare var Image: { new (): HTMLImageElement }}), a member inherited through {@code extends}
     * ({@code HTMLElement extends HTMLOrSVGElement}), a method of a typed variable ({@code console: Console}).
     */
    @Test
    public void testMembersOfLibraryTypes(CodeInsightTestFixture fixture) throws Exception {
        for (String extension : List.of("js", "ts")) {
            assertDeclared(resolve(fixture, extension, "document.tit<caret>le = 'a';\n"), "title", "lib.dom.d.ts");
            assertDeclared(resolve(fixture, extension, "new Image().sr<caret>c = 'a.png';\n"), "src", "lib.dom.d.ts");
            assertDeclared(resolve(fixture, extension, "document.body.foc<caret>us();\n"), "focus", "lib.dom.d.ts");
            assertDeclared(resolve(fixture, extension, "console.lo<caret>g('a');\n"), "log", "lib.dom.d.ts");
        }
    }

    /**
     * The return type of a call ({@code HTMLElement | null} of getElementById), the members of primitive values and
     * arrays, a variable initialized by a call.
     */
    @Test
    public void testCallsAndPrimitives(CodeInsightTestFixture fixture) throws Exception {
        for (String extension : List.of("js", "ts")) {
            assertDeclared(resolve(fixture, extension, "document.getElementById('a').foc<caret>us();\n"), "focus", "lib.dom.d.ts");
            assertDeclared(resolve(fixture, extension, "'a'.toUpperCa<caret>se();\n"), "toUpperCase", "lib.es5.d.ts");
            assertDeclared(resolve(fixture, extension, "[1, 2].joi<caret>n(',');\n"), "join", "lib.es5.d.ts");
            assertDeclared(resolve(fixture, extension, "var s = 'a';\ns.char<caret>At(0);\n"), "charAt", "lib.es5.d.ts");
            assertDeclared(resolve(fixture, extension, "new Date().toISOString().subst<caret>ring(1);\n"), "substring", "lib.es5.d.ts");
        }
    }

    /**
     * The members of the namespaces of the library (Reflect, CSS, Intl - merged from several files), the type a construct
     * signature of a namespace returns ({@code new Intl.DateTimeFormat()}); a member of a namespace isn't global.
     */
    @Test
    public void testNamespaces(CodeInsightTestFixture fixture) throws Exception {
        for (String extension : List.of("js", "ts")) {
            assertDeclared(resolve(fixture, extension, "Reflect.ownK<caret>eys({});\n"), "ownKeys", "lib.es2015.reflect.d.ts");
            assertDeclared(resolve(fixture, extension, "CSS.esc<caret>ape('a');\n"), "escape", "lib.dom.d.ts");
            assertDeclared(resolve(fixture, extension, "Intl.DateTimeF<caret>ormat;\n"), "DateTimeFormat", null);
            assertDeclared(resolve(fixture, extension, "new Intl.DateTimeFormat().form<caret>at(new Date());\n"), "format", "lib.es5.d.ts");
            fixture.configureByText("notglobal." + extension, "new Collat<caret>or();\n");
            assertNull(ReadAction.compute(fixture.getReferenceAtCaretPositionWithAssertion()::resolve), "a member of namespace Intl isn't global");
        }
        assertDeclared(resolve(fixture, "ts", "let c: Intl.Coll<caret>ator;\n"), "Collator", "lib.es5.d.ts");
    }

    /**
     * The type aliases of the library: types for TypeScript, never values for JavaScript.
     */
    @Test
    public void testTypeAliases(CodeInsightTestFixture fixture) throws Exception {
        assertDeclared(resolve(fixture, "ts", "let r: Rec<caret>ord<string, number>;\n"), "Record", "lib.es5.d.ts");
        assertDeclared(resolve(fixture, "ts", "let k: PropertyK<caret>ey;\n"), "PropertyKey", "lib.es5.d.ts");
        assertDeclared(resolve(fixture, "ts", "let l: Intl.LocalesArg<caret>ument;\n"), "LocalesArgument", null);
        fixture.configureByText("alias.js", "Rec<caret>ord;\n");
        assertNull(ReadAction.compute(fixture.getReferenceAtCaretPositionWithAssertion()::resolve), "a type alias isn't a value");
    }

    @Test
    public void testCompletionVariants(CodeInsightTestFixture fixture) throws Exception {
        for (String extension : List.of("js", "ts")) {
            fixture.configureByText("complete." + extension, "setIn<caret>t;\n");
            PsiReference reference = fixture.getReferenceAtCaretPositionWithAssertion();
            Set<String> names = ReadAction.compute(() -> {
                Set<String> result = new HashSet<>();
                for (Object variant : reference.getVariants()) {
                    result.add(variant instanceof LookupElement lookup ? lookup.getLookupString() : String.valueOf(variant));
                }
                return result;
            });
            assertTrue(names.containsAll(List.of("setInterval", "Date", "document")), extension + ": " + names.size() + " variants");
        }
    }

    /**
     * A type annotation names the interface of the library.
     */
    @Test
    public void testTypeReference(CodeInsightTestFixture fixture) throws Exception {
        PsiElement target = resolve(fixture, "ts", "let d: Da<caret>te;\n");
        assertDeclared(target, "Date", "lib.es5.d.ts");
        assertInstanceOf(JSClass.class, target);
    }

    static PsiElement resolve(CodeInsightTestFixture fixture, String extension, String text) throws Exception {
        // a file per text: a test configures several
        fixture.configureByText("app" + Integer.toHexString(text.hashCode()) + "." + extension, text);
        PsiReference reference = fixture.getReferenceAtCaretPositionWithAssertion();
        PsiElement target = ReadAction.compute(reference::resolve);
        assertNotNull(target, "unresolved: " + ReadAction.compute(reference::getCanonicalText));
        // what JSUnresolvedVariableInspection checks
        if (reference instanceof PsiPolyVariantReference poly) {
            boolean valid = false;
            for (ResolveResult result : ReadAction.compute(() -> poly.multiResolve(false))) {
                valid |= result.isValidResult();
            }
            assertTrue(valid, "no valid resolve result");
        }
        return target;
    }

    private static void assertDeclared(PsiElement target, String name, String fileName) {
        assertEquals(name, ReadAction.compute(() -> ((PsiNamedElement) target).getName()));
        if (fileName != null) {
            assertEquals(fileName, ReadAction.compute(() -> target.getContainingFile().getName()));
        }
    }
}

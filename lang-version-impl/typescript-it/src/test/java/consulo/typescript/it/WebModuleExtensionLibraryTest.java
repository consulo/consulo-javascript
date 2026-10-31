package consulo.typescript.it;

import consulo.application.ReadAction;
import consulo.it.CodeInsightTestFixture;
import consulo.it.HeadlessProjectExtension;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiReference;
import consulo.virtualFileSystem.VirtualFile;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * The standard library is a matter of the module's JavaScript extensions: none without one, the ECMAScript library of
 * the language level and the browser libraries of Web.
 */
@ExtendWith(HeadlessProjectExtension.class)
public class WebModuleExtensionLibraryTest {
    @AfterEach
    public void disableWeb(CodeInsightTestFixture fixture) throws Exception {
        WebModules.disableWeb(fixture.getModule());
    }

    @Test
    public void testNoJavaScriptExtensionNoGlobals(CodeInsightTestFixture fixture) throws Exception {
        WebModules.disableWeb(fixture.getModule());
        for (String extension : List.of("js", "ts")) {
            assertUnresolved(fixture, extension, "setInter<caret>val(function () {}, 1);\n");
            assertUnresolved(fixture, extension, "var d = new Da<caret>te();\n");
        }
    }

    @Test
    public void testWebDefaultLibraries(CodeInsightTestFixture fixture) throws Exception {
        WebModules.enableWeb(fixture.getModule());
        for (String extension : List.of("js", "ts")) {
            assertFile(PredefinedLibraryResolveTest.resolve(fixture, extension, "var d = new Da<caret>te();\n"), "lib.es5.d.ts");
            assertFile(PredefinedLibraryResolveTest.resolve(fixture, extension, "docu<caret>ment.title = 'a';\n"), "lib.dom.d.ts");
        }
    }

    @Test
    public void testWebWithoutDom(CodeInsightTestFixture fixture) throws Exception {
        WebModules.enableWeb(fixture.getModule(), Set.of());
        for (String extension : List.of("js", "ts")) {
            assertFile(PredefinedLibraryResolveTest.resolve(fixture, extension, "var d = new Da<caret>te();\n"), "lib.es5.d.ts");
            assertUnresolved(fixture, extension, "docu<caret>ment.title = 'a';\n");
        }
    }

    /**
     * The declarations of a framework (Nuxt's generated auto-imports) come from a JavaScript declarations entry of the
     * module: global without an import, from the moment the entry exists.
     */
    @Test
    public void testDeclarationsEntry(CodeInsightTestFixture fixture) throws Exception {
        WebModules.enableWeb(fixture.getModule());
        VirtualFile declarations = fixture.addFileToProject(
            ".nuxt/types/imports.d.ts",
            "declare function useNuxtClock(): number;\n"
        ).getVirtualFile();
        String text = "var n = useNuxt<caret>Clock();\n";
        for (String extension : List.of("js", "ts")) {
            assertUnresolved(fixture, extension, text);
        }

        WebModules.setDeclarations(fixture.getModule(), "Nuxt", List.of(declarations.getUrl()));
        try {
            for (String extension : List.of("js", "ts")) {
                assertFile(PredefinedLibraryResolveTest.resolve(fixture, extension, text), "imports.d.ts");
            }
        }
        finally {
            WebModules.setDeclarations(fixture.getModule(), "Nuxt", List.of());
        }
    }

    private static void assertUnresolved(CodeInsightTestFixture fixture, String extension, String text) throws Exception {
        fixture.configureByText("app" + Integer.toHexString(text.hashCode()) + "." + extension, text);
        PsiReference reference = fixture.getReferenceAtCaretPositionWithAssertion();
        assertNull(ReadAction.compute(reference::resolve), extension + ": " + text.trim());
    }

    private static void assertFile(PsiElement target, String fileName) {
        assertEquals(fileName, ReadAction.compute(() -> target.getContainingFile().getName()));
    }
}

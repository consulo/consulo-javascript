package consulo.typescript.it;

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSVariable;
import consulo.application.ReadAction;
import consulo.it.CodeInsightTestFixture;
import consulo.it.HeadlessProjectExtension;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.PsiReference;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * TypeScript resolve across files: ES module imports/exports, packages in node_modules and global declarations
 * (the JavaScript stub indexes).
 */
@ExtendWith(HeadlessProjectExtension.class)
public class TypeScriptModuleResolveTest {
    private static final String SRC = CodeInsightTestFixture.SOURCE_ROOT + "/";

    @Test
    public void testNamedImport(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "greeter.ts", "export class Greeter {\n  greet() {}\n}\n");
        PsiElement target = resolve(fixture, "app.ts", "import { Greeter } from './greeter';\nconst g = new Gree<caret>ter();\n");
        assertNamed(target, JSClass.class, "Greeter", "greeter.ts");
    }

    @Test
    public void testAliasedImport(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "util.ts", "export function format(x: string) {\n  return x;\n}\n");
        PsiElement target = resolve(fixture, "app.ts", "import { format as fmt } from './util';\nfm<caret>t('a');\n");
        assertNamed(target, JSFunction.class, "format", "util.ts");
    }

    @Test
    public void testDefaultImport(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "app-component.ts", "export default class AppComponent {\n}\n");
        PsiElement target = resolve(fixture, "main.ts", "import App from './app-component';\nnew Ap<caret>p();\n");
        assertNamed(target, JSClass.class, "AppComponent", "app-component.ts");
    }

    @Test
    public void testNamespaceImport(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "math.ts", "export const PI = 3.14;\n");
        PsiElement target = resolve(fixture, "app.ts", "import * as math from './math';\nconst r = math.P<caret>I * 2;\n");
        assertNamed(target, JSVariable.class, "PI", "math.ts");
    }

    @Test
    public void testReExport(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "lib/impl.ts", "export class Impl {\n}\n");
        fixture.addFileToProject(SRC + "lib/index.ts", "export { Impl } from './impl';\nexport * from './impl';\n");
        PsiElement target = resolve(fixture, "app.ts", "import { Impl } from './lib';\nnew Im<caret>pl();\n");
        assertNamed(target, JSClass.class, "Impl", "impl.ts");
    }

    @Test
    public void testNodeModulesPackage(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject("node_modules/tiny/package.json", "{ \"name\": \"tiny\", \"types\": \"dist/tiny.d.ts\" }");
        fixture.addFileToProject("node_modules/tiny/dist/tiny.d.ts", "export declare function tiny(): void;\n");
        PsiElement target = resolve(fixture, "app.ts", "import { tiny } from 'tiny';\nti<caret>ny();\n");
        assertNamed(target, JSFunction.class, "tiny", "tiny.d.ts");
    }

    @Test
    public void testMemberOfImportedClass(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "greeter.ts", "export class Greeter {\n  greet() {}\n}\n");
        PsiElement target = resolve(fixture, "app.ts", "import { Greeter } from './greeter';\nconst g = new Greeter();\ng.gre<caret>et();\n");
        assertNamed(target, JSFunction.class, "greet", "greeter.ts");
    }

    @Test
    public void testGlobalDeclarationOfScript(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "globals.ts", "function globalHelper() {\n}\n");
        PsiElement target = resolve(fixture, "app.ts", "globalHel<caret>per();\n");
        assertNamed(target, JSFunction.class, "globalHelper", "globals.ts");
    }

    @Test
    public void testModuleDeclarationIsNotGlobal(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "hidden.ts", "export function hiddenHelper() {\n}\n");
        PsiFile file = fixture.configureByText("app.ts", "hiddenHel<caret>per();\n");
        PsiReference reference = fixture.getReferenceAtCaretPositionWithAssertion();
        assertNull(ReadAction.compute(reference::resolve), "a module's declaration needs an import");
    }

    @Test
    public void testImportedType(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "model.ts", "export interface User {\n  name: string;\n}\n");
        PsiElement target = resolve(fixture, "app.ts", "import { User } from './model';\nlet u: Us<caret>er;\n");
        assertNamed(target, JSClass.class, "User", "model.ts");
    }

    @Test
    public void testGlobalInterfaceOfDeclarationFile(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "globals.d.ts", "interface Window2 {\n}\n");
        PsiElement target = resolve(fixture, "app.ts", "let w: Win<caret>dow2;\n");
        assertNamed(target, JSClass.class, "Window2", "globals.d.ts");
    }

    @Test
    public void testNamespaceImportType(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "model.ts", "export class Order {\n}\n");
        PsiElement target = resolve(fixture, "app.ts", "import * as m from './model';\nlet o: m.Or<caret>der;\n");
        assertNamed(target, JSClass.class, "Order", "model.ts");
    }

    @Test
    public void testLocalAndNamespaceVariants(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "math.ts", "export const PI = 3.14;\nexport function sqrt(x: number) {\n  return x;\n}\n");
        fixture.configureByText("app.ts", "import * as math from './math';\nconst radius = 2;\nfunction area() {\n  return r<caret>a;\n}\n");
        PsiReference local = fixture.getReferenceAtCaretPositionWithAssertion();
        java.util.Set<String> names = lookupStrings(local);
        org.junit.jupiter.api.Assertions.assertTrue(names.containsAll(java.util.Set.of("radius", "area", "math")), names.toString());

        fixture.configureByText("app2.ts", "import * as math from './math';\nmath.<caret>s;\n");
        assertEquals(java.util.Set.of("PI", "sqrt"), lookupStrings(fixture.getReferenceAtCaretPositionWithAssertion()));
    }

    @Test
    public void testMemberAndEnumVariants(CodeInsightTestFixture fixture) throws Exception {
        fixture.configureByText("counter.ts", """
            class Base {
                base() {
                }
            }
            class Counter extends Base {
                count = 0;
                inc() {
                    this.<caret>c;
                }
            }
            """);
        assertEquals(java.util.Set.of("count", "inc", "base"), lookupStrings(fixture.getReferenceAtCaretPositionWithAssertion()));

        fixture.configureByText("color.ts", "enum Color { Red, Green }\nconst c = Color.<caret>R;\n");
        assertEquals(java.util.Set.of("Red", "Green"), lookupStrings(fixture.getReferenceAtCaretPositionWithAssertion()));
    }

    private static java.util.Set<String> lookupStrings(PsiReference reference) {
        return ReadAction.compute(() -> {
            java.util.Set<String> names = new java.util.TreeSet<>();
            for (Object variant : reference.getVariants()) {
                names.add(variant instanceof consulo.language.editor.completion.lookup.LookupElement lookup ? lookup.getLookupString() : String.valueOf(variant));
            }
            return names;
        });
    }

    private static PsiElement resolve(CodeInsightTestFixture fixture, String fileName, String text) throws Exception {
        fixture.configureByText(fileName, text);
        PsiReference reference = fixture.getReferenceAtCaretPositionWithAssertion();
        PsiElement target = ReadAction.compute(reference::resolve);
        assertNotNull(target, "unresolved: " + ReadAction.compute(reference::getCanonicalText));
        return target;
    }

    private static void assertNamed(PsiElement target, Class<?> type, String name, String fileName) {
        assertInstanceOf(type, target);
        assertEquals(name, ReadAction.compute(() -> ((PsiNamedElement) target).getName()));
        assertEquals(fileName, ReadAction.compute(() -> target.getContainingFile().getName()));
    }
}

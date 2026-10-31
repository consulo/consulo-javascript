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
    public void testDecoratedExportImport(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "app.component.ts",
            "@Component({ selector: 'app-root', templateUrl: './app.component.html' })\nexport class AppComponent {\n  title = 'x';\n}\n");
        fixture.addFileToProject(SRC + "service.ts", "@Injectable()\nexport default class Service {\n}\n");
        PsiElement component = resolve(fixture, "main.ts", "import { AppComponent } from './app.component';\nnew AppComp<caret>onent();\n");
        assertNamed(component, JSClass.class, "AppComponent", "app.component.ts");
        PsiElement service = resolve(fixture, "main2.ts", "import Service from './service';\nnew Serv<caret>ice();\n");
        assertNamed(service, JSClass.class, "Service", "service.ts");
    }

    /**
     * {@code declare global} of a module (Nuxt's generated auto-imports): global, while the module's own declarations
     * aren't.
     */
    @Test
    public void testDeclareGlobalInModule(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "types/imports.d.ts",
            "export {}\ndeclare global {\n  const useCounter: () => number\n  interface NuxtApp {\n  }\n}\nexport const notGlobal = 1;\n");
        PsiElement counter = resolve(fixture, "use.ts", "const n = useCoun<caret>ter();\n");
        assertNamed(counter, JSVariable.class, "useCounter", "imports.d.ts");
        PsiElement app = resolve(fixture, "app.ts", "let a: NuxtA<caret>pp;\n");
        assertNamed(app, JSClass.class, "NuxtApp", "imports.d.ts");
        fixture.configureByText("other.ts", "notGlo<caret>bal;\n");
        assertNull(ReadAction.compute(fixture.getReferenceAtCaretPositionWithAssertion()::resolve), "a module's own declaration needs an import");
    }

    /**
     * The module specifier of an import / export ... from is a reference to the module file.
     */
    @Test
    public void testModuleSpecifierReference(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "greeter.ts", "export class Greeter {\n}\n");
        fixture.addFileToProject("node_modules/tiny/package.json", "{ \"name\": \"tiny\", \"types\": \"dist/tiny.d.ts\" }");
        fixture.addFileToProject("node_modules/tiny/dist/tiny.d.ts", "export declare function tiny(): void;\n");
        assertFileTarget(resolve(fixture, "spec1.ts", "import { Greeter } from './gre<caret>eter';\n"), "greeter.ts");
        assertFileTarget(resolve(fixture, "spec2.ts", "import { tiny } from 'ti<caret>ny';\n"), "tiny.d.ts");
        assertFileTarget(resolve(fixture, "spec3.ts", "export { Greeter } from './gre<caret>eter';\n"), "greeter.ts");
    }

    private static void assertFileTarget(PsiElement target, String fileName) {
        assertInstanceOf(PsiFile.class, target);
        assertEquals(fileName, ReadAction.compute(() -> ((PsiFile) target).getName()));
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

    /**
     * An imported name exported again ({@code index.d.ts} of packages) is a re-export of the other module.
     */
    @Test
    public void testExportOfImportedName(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "parts/impl.ts", "export class Impl {\n}\nexport default class Main {\n}\n");
        fixture.addFileToProject(SRC + "parts/index.ts",
            "import { Impl as Inner } from './impl';\nimport Main from './impl';\nexport { Inner as Impl };\nexport default Main;\n");
        assertNamed(resolve(fixture, "app.ts", "import { Impl } from './parts';\nnew Im<caret>pl();\n"), JSClass.class, "Impl", "impl.ts");
        assertNamed(resolve(fixture, "app2.ts", "import Main from './parts';\nnew Ma<caret>in();\n"), JSClass.class, "Main", "impl.ts");
    }

    /**
     * One module re-exported both by {@code export *} and under another name: looked up once per name.
     */
    @Test
    public void testStarAndRenamedReExportOfOneModule(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "both/util.ts", "export function make() {\n}\n");
        fixture.addFileToProject(SRC + "both/index.ts", "export * from './util';\nexport { make as build } from './util';\n");
        assertNamed(resolve(fixture, "app.ts", "import { build } from './both';\nbui<caret>ld();\n"), JSFunction.class, "make", "util.ts");
    }

    @Test
    public void testExportStarAsNamespace(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "ns/impl.ts", "export class Impl {\n}\n");
        fixture.addFileToProject(SRC + "ns/index.ts", "export * as impl from './impl';\n");
        assertFileTarget(resolve(fixture, "app.ts", "import { impl } from './ns';\nim<caret>pl;\n"), "impl.ts");
    }

    /**
     * {@code export = express} of CommonJS declaration files: the entity is the default export, the members of its
     * namespace the named ones.
     */
    @Test
    public void testExportEquals(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject("node_modules/express/package.json", "{ \"name\": \"express\", \"types\": \"index.d.ts\" }");
        fixture.addFileToProject("node_modules/express/index.d.ts",
            "declare function express(): void;\ndeclare namespace express {\n  function json(): void;\n}\nexport = express;\n");
        PsiElement json = resolve(fixture, "app.ts", "import { json } from 'express';\njs<caret>on();\n");
        assertNamed(json, JSFunction.class, "json", "index.d.ts");
        PsiElement entity = resolve(fixture, "app2.ts", "import express from 'express';\nexpr<caret>ess();\n");
        assertEquals("express", ReadAction.compute(() -> ((PsiNamedElement) entity).getName()));
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

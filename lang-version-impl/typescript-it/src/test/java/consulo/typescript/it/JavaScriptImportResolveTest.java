/*
 * Copyright 2013-2026 consulo.io
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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

/**
 * The names imported by plain JavaScript resolve to the declarations of the module: its declarations (.d.ts) first.
 */
@ExtendWith(HeadlessProjectExtension.class)
public class JavaScriptImportResolveTest {
    private static final String SRC = CodeInsightTestFixture.SOURCE_ROOT + "/";

    @Test
    public void testNamedImport(CodeInsightTestFixture fixture) throws Exception {
        addTinyModule(fixture);
        PsiElement target = resolve(fixture, "main.js", "import { tiny } from './tiny';\nti<caret>ny();\n");
        assertNamed(target, JSFunction.class, "tiny", "tiny.d.ts");
    }

    @Test
    public void testAliasedImport(CodeInsightTestFixture fixture) throws Exception {
        addTinyModule(fixture);
        PsiElement target = resolve(fixture, "main.js", "import { tiny as t } from './tiny';\n<caret>t();\n");
        assertNamed(target, JSFunction.class, "tiny", "tiny.d.ts");
    }

    @Test
    public void testDefaultImport(CodeInsightTestFixture fixture) throws Exception {
        addTinyModule(fixture);
        PsiElement target = resolve(fixture, "main.js", "import Tiny from './tiny';\nnew Ti<caret>ny();\n");
        assertNamed(target, JSClass.class, "Tiny", "tiny.d.ts");
    }

    @Test
    public void testNamespaceImport(CodeInsightTestFixture fixture) throws Exception {
        addTinyModule(fixture);
        PsiElement target = resolve(fixture, "main.js", "import * as all from './tiny';\na<caret>ll;\n");
        assertInstanceOf(PsiFile.class, target);
        assertEquals("tiny.d.ts", ReadAction.compute(() -> ((PsiFile) target).getName()));
    }

    @Test
    public void testRelativeImportOfDeclarations(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "lib.d.ts", "export declare function helper(): void;\n");
        PsiElement target = resolve(fixture, "main.js", "import { helper } from './lib';\nhel<caret>per();\n");
        assertNamed(target, JSFunction.class, "helper", "lib.d.ts");
    }

    @Test
    public void testModuleSpecifierReference(CodeInsightTestFixture fixture) throws Exception {
        addTinyModule(fixture);
        PsiElement target = resolve(fixture, "main.js", "import { tiny } from './ti<caret>ny';\n");
        assertInstanceOf(PsiFile.class, target);
    }

    @Test
    public void testJavaScriptExportedFunction(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "util.js", "export function helper() {}\n");
        PsiElement target = resolve(fixture, "main.js", "import { helper } from './util';\nhel<caret>per();\n");
        assertNamed(target, JSFunction.class, "helper", "util.js");
    }

    @Test
    public void testJavaScriptDefaultClass(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "store.js", "export default class Store {}\n");
        PsiElement target = resolve(fixture, "main.js", "import S from './store';\nnew <caret>S();\n");
        assertNamed(target, JSClass.class, "Store", "store.js");
    }

    @Test
    public void testJavaScriptLocalExportList(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "math.js", "function sum(a, b) { return a + b; }\nexport { sum as add };\n");
        PsiElement target = resolve(fixture, "main.js", "import { add } from './math';\na<caret>dd(1, 2);\n");
        assertNamed(target, JSFunction.class, "sum", "math.js");
    }

    @Test
    public void testJavaScriptReExports(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "lib/a.js", "export const alpha = 1;\n");
        fixture.addFileToProject(SRC + "lib/b.js", "export function beta() {}\nexport default function gamma() {}\n");
        fixture.addFileToProject(SRC + "lib/index.js", "export { alpha as first } from './a';\nexport * from './b';\n");
        PsiElement first = resolve(fixture, "main.js", "import { first } from './lib';\nfir<caret>st;\n");
        assertNamed(first, JSVariable.class, "alpha", "a.js");
        PsiElement beta = resolve(fixture, "main2.js", "import { beta } from './lib';\nbe<caret>ta();\n");
        assertNamed(beta, JSFunction.class, "beta", "b.js");
    }

    @Test
    public void testExportAllSkipsDefault(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "lib/b.js", "export default function gamma() {}\n");
        fixture.addFileToProject(SRC + "lib/index.js", "export * from './b';\n");
        fixture.configureByText("main.js", "import g from './lib';\n<caret>g();\n");
        PsiReference reference = fixture.getReferenceAtCaretPositionWithAssertion();
        PsiElement target = ReadAction.compute(reference::resolve);
        // the binding itself: export * doesn't re-export the default export
        assertEquals("ES6ImportedBindingImpl", target == null ? null : target.getClass().getSimpleName());
    }

    /**
     * A module with declarations next to its code: the import resolves to the declarations (the packages of npm are
     * resolved by the Node.js plugin)
     */
    private static void addTinyModule(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "tiny.d.ts", "export declare function tiny(): void;\nexport default class Tiny {}\n");
        fixture.addFileToProject(SRC + "tiny.js", "export function tiny() {}\n");
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

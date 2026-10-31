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
import consulo.application.ReadAction;
import consulo.application.WriteAction;
import consulo.disposer.Disposable;
import consulo.disposer.Disposer;
import consulo.it.CodeInsightTestFixture;
import consulo.it.HeadlessProjectExtension;
import consulo.language.impl.internal.psi.PsiManagerEx;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiManager;
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.PsiReference;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * The imports of plain JavaScript resolve by the export tables of the stubs: the modules aren't parsed - re-exports,
 * {@code export *}, an imported name exported again and a default export.
 */
@ExtendWith(HeadlessProjectExtension.class)
public class JavaScriptModuleExportsStubOnlyTest {
    private static final String SRC = CodeInsightTestFixture.SOURCE_ROOT + "/";

    @Test
    public void testModuleAstNotLoaded(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject(SRC + "lib/index.js",
            "import { Widget as W } from './widget';\nexport { W as Widget };\nexport * from './util';\n"
                + "export { make as build } from './util';\nexport default function main() {}\n");
        fixture.addFileToProject(SRC + "lib/widget.js", "export class Widget {\n}\n");
        fixture.addFileToProject(SRC + "lib/util.js", "export function make() {}\nexport function helper() {}\n");

        Map<String, String> cases = Map.of(
            "import { Widget } from './lib/index';\nnew Wid<caret>get();\n", "Widget",
            "import { helper } from './lib/index';\nhel<caret>per();\n", "helper",
            "import { build } from './lib/index';\nbui<caret>ld();\n", "make",
            "import main from './lib/index';\nma<caret>in();\n", "main"
        );
        // index the modules (the stubs are built from their tree), then forget that PSI
        for (String text : cases.keySet()) {
            resolve(fixture, text);
        }
        WriteAction.run(() -> PsiManager.getInstance(fixture.getProject()).dropPsiCaches());
        Disposable disposable = Disposable.newDisposable();
        // fails the resolve that loads the tree of a module
        ((PsiManagerEx) PsiManager.getInstance(fixture.getProject())).setAssertOnFileLoadingFilter(
            file -> file.getPath().contains("/lib/"),
            disposable
        );
        try {
            for (Map.Entry<String, String> entry : cases.entrySet()) {
                PsiElement target = resolve(fixture, entry.getKey());
                assertEquals(entry.getValue(), ReadAction.compute(() -> ((PsiNamedElement) target).getName()));
                Class<?> type = entry.getValue().equals("Widget") ? JSClass.class : JSFunction.class;
                assertInstanceOf(type, target);
            }
        }
        finally {
            Disposer.dispose(disposable);
        }
    }

    private static PsiElement resolve(CodeInsightTestFixture fixture, String text) throws Exception {
        fixture.configureByText("app" + Integer.toHexString(text.hashCode()) + ".js", text);
        PsiReference reference = fixture.getReferenceAtCaretPositionWithAssertion();
        PsiElement target = ReadAction.compute(reference::resolve);
        assertNotNull(target, text.trim());
        return target;
    }
}

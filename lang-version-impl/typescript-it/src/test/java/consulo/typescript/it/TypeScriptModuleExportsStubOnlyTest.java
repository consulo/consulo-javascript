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
 * An import resolves by the export table in the stub of the module file, without parsing it: re-exports, imported
 * names exported again, {@code export =}. A class of its own: the project is shared by the tests of a class.
 */
@ExtendWith(HeadlessProjectExtension.class)
public class TypeScriptModuleExportsStubOnlyTest {
    @Test
    public void testModuleAstNotLoaded(CodeInsightTestFixture fixture) throws Exception {
        fixture.addFileToProject("node_modules/pkg/package.json", "{ \"name\": \"pkg\", \"types\": \"index.d.ts\" }");
        fixture.addFileToProject("node_modules/pkg/index.d.ts",
            "import { Widget as W } from './widget';\nexport { W as Widget };\nexport * from './util';\nexport { make } from './util';\n");
        fixture.addFileToProject("node_modules/pkg/widget.d.ts", "export declare class Widget {\n}\n");
        fixture.addFileToProject("node_modules/pkg/util.d.ts", "export declare function make(): void;\nexport declare function helper(): void;\n");
        fixture.addFileToProject("node_modules/cjs/package.json", "{ \"name\": \"cjs\", \"types\": \"index.d.ts\" }");
        fixture.addFileToProject("node_modules/cjs/index.d.ts",
            "declare function cjs(): void;\ndeclare namespace cjs {\n  function json(): void;\n}\nexport = cjs;\n");

        Map<String, String> cases = Map.of(
            "import { Widget } from 'pkg';\nnew Wid<caret>get();\n", "Widget",
            "import { helper } from 'pkg';\nhel<caret>per();\n", "helper",
            "import { make } from 'pkg';\nma<caret>ke();\n", "make",
            "import { json } from 'cjs';\njs<caret>on();\n", "json"
        );
        // index the declaration files (the stubs are built from their AST), then forget that PSI
        for (String text : cases.keySet()) {
            resolve(fixture, text);
        }
        WriteAction.run(() -> PsiManager.getInstance(fixture.getProject()).dropPsiCaches());

        Disposable disposable = Disposable.newDisposable();
        // fails the resolve that loads the AST of a declaration file
        ((PsiManagerEx) PsiManager.getInstance(fixture.getProject())).setAssertOnFileLoadingFilter(file -> file.getName().endsWith(".d.ts"), disposable);
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
        fixture.configureByText("app" + Integer.toHexString(text.hashCode()) + ".ts", text);
        PsiReference reference = fixture.getReferenceAtCaretPositionWithAssertion();
        PsiElement target = ReadAction.compute(reference::resolve);
        assertNotNull(target, text.trim());
        return target;
    }
}

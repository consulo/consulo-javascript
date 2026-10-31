package consulo.typescript.it;

import consulo.application.ReadAction;
import consulo.application.WriteAction;
import consulo.disposer.Disposable;
import consulo.disposer.Disposer;
import consulo.it.CodeInsightTestFixture;
import consulo.it.HeadlessProjectExtension;
import consulo.language.impl.internal.psi.PsiManagerEx;
import consulo.language.impl.psi.PsiFileImpl;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;

/**
 * The globals and the members of their types (methods, properties, constructed and inherited types) resolve by the
 * stubs: the large declaration files (lib.dom.d.ts) aren't parsed. A class of its own: the project is shared by the tests of a class, another test may load an AST (a
 * property signature isn't stubbed).
 */
@ExtendWith(HeadlessProjectExtension.class)
public class PredefinedLibraryStubOnlyTest {
    /**
     * The standard library comes with the Web extension of the module.
     */
    @BeforeEach
    public void enableWeb(CodeInsightTestFixture fixture) throws Exception {
        WebModules.enableWeb(fixture.getModule());
    }

    @Test
    public void testLibraryAstNotLoaded(CodeInsightTestFixture fixture) throws Exception {
        // the headless project indexes the library files on the first query, building their stubs from the AST: index
        // them, then forget that PSI
        PredefinedLibraryResolveTest.resolve(fixture, "js", "setInter<caret>val(function () {}, 1);\n");
        WriteAction.run(() -> PsiManager.getInstance(fixture.getProject()).dropPsiCaches());

        Disposable disposable = Disposable.newDisposable();
        // fails the resolve that loads the AST of a library file
        ((PsiManagerEx) PsiManager.getInstance(fixture.getProject()))
            .setAssertOnFileLoadingFilter(file -> file.getName().startsWith("lib.") && file.getName().endsWith(".d.ts"), disposable);
        try {
            for (String extension : List.of("js", "ts")) {
                for (String text : List.of(
                    "new XMLHttpRequest().op<caret>en('GET', '/');\n",
                    "new Date().getT<caret>ime();\n",
                    "setInter<caret>val(function () {}, 1);\n",
                    "new Promise(function (r) {}).th<caret>en();\n",
                    "document.tit<caret>le = 'a';\n",
                    "new Image().sr<caret>c = 'a.png';\n",
                    "document.body.foc<caret>us();\n",
                    "console.lo<caret>g('a');\n",
                    "document.getElementById('a').foc<caret>us();\n",
                    "'a'.toUpperCa<caret>se();\n",
                    "Reflect.ownK<caret>eys({});\n",
                    "new Intl.DateTimeFormat().form<caret>at(new Date());\n"
                )) {
                    PsiElement target = PredefinedLibraryResolveTest.resolve(fixture, extension, text);
                    PsiFile file = ReadAction.compute(target::getContainingFile);
                    assertFalse(((PsiFileImpl) file).isContentsLoaded(), extension + ": " + text.trim() + " loaded the AST of " + file.getName());
                }
            }
        }
        finally {
            Disposer.dispose(disposable);
        }
    }
}

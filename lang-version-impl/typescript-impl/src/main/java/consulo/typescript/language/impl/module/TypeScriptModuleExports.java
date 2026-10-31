package consulo.typescript.language.impl.module;

import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.module.resolve.JavaScriptModuleExports;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.typescript.language.TypeScriptLanguage;
import consulo.typescript.language.impl.psi.TypeScriptModuleUtil;

import java.util.List;
import java.util.Set;

/**
 * The exports of TypeScript modules and declaration files, by the export table of their file stub.
 *
 * @since 2026-10-31
 */
@ExtensionImpl
public class TypeScriptModuleExports implements JavaScriptModuleExports {
    @Override
    public boolean accepts(PsiFile module) {
        return module.getLanguage() == TypeScriptLanguage.INSTANCE;
    }

    @Override
    @RequiredReadAction
    public void collectExports(PsiFile module, String name, List<PsiElement> result, Set<JavaScriptModuleExports.Visit> visited) {
        TypeScriptModuleUtil.collectExports(module, name, result, visited);
    }
}

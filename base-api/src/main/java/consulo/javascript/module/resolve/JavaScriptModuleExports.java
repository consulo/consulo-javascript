package consulo.javascript.module.resolve;

import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ComponentScope;
import consulo.annotation.component.ExtensionAPI;
import consulo.application.Application;
import consulo.application.progress.ProgressManager;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * The declarations a module file exports under a name, by the language of the file: what {@code import {a} from './x'}
 * imports. Re-exports ({@code export {a} from './y'}, {@code export * from './y'}) go to {@link #collect}, so a chain
 * crosses languages (a JavaScript module re-exporting a declaration file). A module is visited once per name: one
 * module re-exports another both with {@code export *} and under another name ({@code export {make as build}}).
 * <p>
 * Resolving an import must not parse the module file: an implementation reads the exports from the stub of the file.
 *
 * @since 2026-10-31
 */
@ExtensionAPI(ComponentScope.APPLICATION)
public interface JavaScriptModuleExports {
    /**
     * The name of {@code export default}.
     */
    String DEFAULT_EXPORT = "default";

    /**
     * A module looked up for a name (the cycles of re-exports).
     */
    record Visit(PsiFile module, String name) {
    }

    /**
     * Whether the module file is of this provider (its language).
     */
    boolean accepts(PsiFile module);

    /**
     * Adds the declarations the module exports under the name. Re-exports of other modules go to
     * {@link #collect(PsiFile, String, List, Set)} with the same {@code visited}.
     */
    @RequiredReadAction
    void collectExports(PsiFile module, String name, List<PsiElement> result, Set<Visit> visited);

    /**
     * The declarations a module exports under a name ({@link #DEFAULT_EXPORT} for {@code export default}).
     */
    @RequiredReadAction
    static List<PsiElement> findExports(PsiFile module, String name) {
        List<PsiElement> result = new ArrayList<>();
        collect(module, name, result, new HashSet<>());
        return result;
    }

    /**
     * Adds the exports of a module by the provider of its file; once per module and name (cyclic re-exports).
     */
    @RequiredReadAction
    static void collect(PsiFile module, String name, List<PsiElement> result, Set<Visit> visited) {
        if (!visited.add(new Visit(module, name))) {
            return;
        }
        ProgressManager.checkCanceled();
        for (JavaScriptModuleExports provider : Application.get().getExtensionList(JavaScriptModuleExports.class)) {
            if (provider.accepts(module)) {
                provider.collectExports(module, name, result, visited);
                return;
            }
        }
    }
}

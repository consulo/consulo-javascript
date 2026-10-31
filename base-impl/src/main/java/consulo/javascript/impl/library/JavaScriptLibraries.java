package consulo.javascript.impl.library;

import consulo.annotation.access.RequiredReadAction;
import consulo.application.util.CachedValue;
import consulo.application.util.CachedValueProvider;
import consulo.application.util.CachedValuesManager;
import consulo.content.base.BinariesOrderRootType;
import consulo.content.bundle.Sdk;
import consulo.javascript.library.JavaScriptStandardLibrary;
import consulo.javascript.module.extension.JavaScriptModuleExtension;
import consulo.javascript.module.declarations.JavaScriptDeclarations;
import consulo.javascript.module.extension.JavaScriptModuleExtensions;
import consulo.language.psi.PsiElement;
import consulo.language.psi.scope.GlobalSearchScope;
import consulo.language.util.ModuleUtilCore;
import consulo.language.version.LanguageVersion;
import consulo.module.Module;
import consulo.module.content.ModuleRootManager;
import consulo.module.content.ProjectRootManager;
import consulo.project.Project;
import consulo.util.dataholder.Key;
import consulo.virtualFileSystem.VirtualFile;
import consulo.virtualFileSystem.VirtualFileManager;
import org.jspecify.annotations.Nullable;

import java.util.LinkedHashSet;
import java.util.Set;

/**
 * The global declaration files JavaScript code sees: by the module of its file, the SDKs of the module's JavaScript
 * extensions - of a lib directory only the libraries of the language level and environment libraries of the extensions
 * ({@code lib.es2021.d.ts}, {@code lib.dom.d.ts} of Web), other roots whole ({@code @types/node}) - and the roots of
 * its {@link JavaScriptDeclarations} entries (frameworks). A module without a JavaScript extension sees none.
 *
 * @since 2026-10-31
 */
public final class JavaScriptLibraries {
    /**
     * The visible files and the lib files of the SDKs the module doesn't see (the DOM without Web's DOM libraries).
     */
    private record Scopes(GlobalSearchScope visible, GlobalSearchScope hidden) {
    }

    /**
     * Per module: follows its module extensions and SDKs - recomputed on root changes - and files created later (Nuxt
     * generates its {@code .nuxt/types/imports.d.ts} after the project opens) - recomputed on VFS structure changes.
     */
    private static final Key<CachedValue<Scopes>> MODULE_SCOPES = Key.create("JavaScriptLibraries.MODULE_SCOPES");

    private static final Set<String> SOURCE_EXTENSIONS = Set.of("ts", "js", "mjs", "cjs", "mts", "cts");

    private JavaScriptLibraries() {
    }

    /**
     * The scope of the global declarations seen from the element - by the module of its file, also for a script
     * embedded in another file or an injected expression - or null when it sees none.
     */
    @Nullable
    @RequiredReadAction
    public static GlobalSearchScope getScope(PsiElement element) {
        Module module = ModuleUtilCore.findModuleForPsiElement(element);
        return module == null ? null : getScope(module);
    }

    @Nullable
    @RequiredReadAction
    public static GlobalSearchScope getScope(Module module) {
        GlobalSearchScope scope = getScopes(module).visible();
        return scope == GlobalSearchScope.EMPTY_SCOPE ? null : scope;
    }

    /**
     * The resolve scope of the element without the lib files of the SDKs its module doesn't see: the SDK is in the
     * resolve scope whole, the libraries are chosen by the module extensions.
     */
    @RequiredReadAction
    public static GlobalSearchScope restrictResolveScope(PsiElement element, GlobalSearchScope resolveScope) {
        Module module = ModuleUtilCore.findModuleForPsiElement(element);
        GlobalSearchScope hidden = module == null ? GlobalSearchScope.EMPTY_SCOPE : getScopes(module).hidden();
        return hidden == GlobalSearchScope.EMPTY_SCOPE ? resolveScope : resolveScope.intersectWith(GlobalSearchScope.notScope(hidden));
    }

    @RequiredReadAction
    private static Scopes getScopes(Module module) {
        Project project = module.getProject();
        return CachedValuesManager.getManager(project).getCachedValue(
            module,
            MODULE_SCOPES,
            () -> CachedValueProvider.Result.create(
                computeScopes(module),
                ProjectRootManager.getInstance(project),
                VirtualFileManager.VFS_STRUCTURE_MODIFICATIONS
            ),
            false
        );
    }

    @RequiredReadAction
    private static Scopes computeScopes(Module module) {
        Set<VirtualFile> visible = new LinkedHashSet<>();
        Set<VirtualFile> hidden = new LinkedHashSet<>();

        LanguageVersion languageVersion = JavaScriptModuleExtensions.getLanguageVersion(module);
        if (languageVersion != null) {
            JavaScriptStandardLibrary standardLibrary = JavaScriptStandardLibrary.getInstance();
            String level = standardLibrary.getEcmaScriptLevel(languageVersion);
            Set<String> libraries = JavaScriptModuleExtensions.getStandardLibraries(module);
            for (JavaScriptModuleExtension<?> extension : JavaScriptModuleExtensions.getExtensions(module)) {
                Sdk sdk = extension.getSdk();
                if (sdk == null) {
                    continue;
                }
                for (VirtualFile root : sdk.getRootProvider().getFiles(BinariesOrderRootType.ID)) {
                    if (JavaScriptStandardLibrary.isLibDirectory(root)) {
                        Set<VirtualFile> selected = level == null ? Set.of() : Set.copyOf(standardLibrary.collectFiles(root, level, libraries));
                        visible.addAll(selected);
                        for (VirtualFile child : root.getChildren()) {
                            if (!child.isDirectory() && !selected.contains(child)) {
                                hidden.add(child);
                            }
                        }
                    }
                    else {
                        collectSources(root, visible);
                    }
                }
            }
        }

        // the declarations of the frameworks (Nuxt's auto-imports), generated files show up once they exist
        for (VirtualFile root : JavaScriptDeclarations.getRoots(ModuleRootManager.getInstance(module))) {
            collectSources(root, visible);
        }

        Project project = module.getProject();
        return new Scopes(
            visible.isEmpty() ? GlobalSearchScope.EMPTY_SCOPE : GlobalSearchScope.filesScope(project, visible),
            hidden.isEmpty() ? GlobalSearchScope.EMPTY_SCOPE : GlobalSearchScope.filesScope(project, hidden)
        );
    }

    /**
     * The JavaScript and TypeScript files of an SDK root ({@code @types/node}).
     */
    private static void collectSources(VirtualFile file, Set<VirtualFile> result) {
        if (!file.isDirectory()) {
            String extension = file.getExtension();
            if (extension != null && SOURCE_EXTENSIONS.contains(extension)) {
                result.add(file);
            }
            return;
        }
        for (VirtualFile child : file.getChildren()) {
            collectSources(child, result);
        }
    }
}

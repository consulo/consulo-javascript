package consulo.typescript.language.impl.psi;

import com.intellij.lang.javascript.psi.resolve.JSImportedElementResolveResult;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.ecmascript.psi.impl.resolve.EcmaScript6ResolveHelper;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiNamedElement;
import org.jspecify.annotations.Nullable;

/**
 * The resolve helper of TypeScript: the one of ECMAScript 6, except that a global script (the stub of the file knows
 * it, {@link TypeScriptModuleUtil#isModule}) has no imports to look at - the declarations of lib.dom.d.ts don't parse
 * the file for them.
 *
 * @since 2026-10-31
 */
public class TypeScriptResolveHelper extends EcmaScript6ResolveHelper {
    public static final TypeScriptResolveHelper INSTANCE = new TypeScriptResolveHelper();

    @Nullable
    @Override
    @RequiredReadAction
    public JSImportedElementResolveResult resolveTypeNameUsingImports(String referencedName, PsiNamedElement parent) {
        PsiFile file = parent.getContainingFile();
        if (file != null && !TypeScriptModuleUtil.isModule(file)) {
            return null;
        }
        return super.resolveTypeNameUsingImports(referencedName, parent);
    }
}

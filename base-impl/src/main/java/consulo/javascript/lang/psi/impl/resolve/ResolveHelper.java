package consulo.javascript.lang.psi.impl.resolve;

import com.intellij.lang.javascript.psi.impl.JSReferenceExpressionImpl;
import com.intellij.lang.javascript.psi.resolve.JSImportedElementResolveResult;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.version.LanguageVersion;
import jakarta.annotation.Nonnull;

import jakarta.annotation.Nullable;

/**
 * @author VISTALL
 * @since 2019-12-14
 */
public abstract class ResolveHelper {
    private static final ResolveHelper DEFAULT = new ResolveHelper() {
    };

    public static ResolveHelper find(PsiElement element) {
        LanguageVersion languageVersion = element.getLanguageVersion();
        return languageVersion instanceof JavaScriptVersionWithHelper javaScriptVersionWithHelper
            ? javaScriptVersionWithHelper.getHelper()
            : DEFAULT;
    }

    @Nullable
    public JSImportedElementResolveResult resolveTypeNameUsingImports(final @Nonnull String referencedName, PsiNamedElement parent) {
        return null;
    }

    public boolean execute(ResolveProcessor resolveProcessor, PsiElement element, ResolveState state) {
        return false;
    }

    @RequiredReadAction
    public boolean isResolveTo(JSReferenceExpressionImpl expression, PsiElement element) {
        return false;
    }
}

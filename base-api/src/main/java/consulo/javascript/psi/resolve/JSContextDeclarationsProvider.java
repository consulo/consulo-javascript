package consulo.javascript.psi.resolve;

import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ComponentScope;
import consulo.annotation.component.ExtensionAPI;
import consulo.component.extension.ExtensionPointName;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.resolve.PsiScopeProcessor;

/**
 * Declarations visible to a JavaScript file that lives in another file: injected or embedded code, like the template
 * expressions of a Vue component that see the component's script.
 * <p>
 * Asked after the JavaScript file's own declarations, before the declarations of the host file.
 *
 * @since 2026-10-31
 */
@ExtensionAPI(ComponentScope.APPLICATION)
public interface JSContextDeclarationsProvider {
    ExtensionPointName<JSContextDeclarationsProvider> EP_NAME = ExtensionPointName.create(JSContextDeclarationsProvider.class);

    /**
     * @param jsFile  the injected JavaScript file; for a script embedded into markup ({@code <script>} of HTML), the
     *                markup file
     * @param context the element of the host file holding it ({@link PsiFile#getContext()}); for an embedded script,
     *                the markup file
     * @param place   the element resolved or completed
     * @return {@code false} to stop processing
     */
    @RequiredReadAction
    boolean processDeclarations(PsiScopeProcessor processor, PsiFile jsFile, PsiElement context, PsiElement place);
}

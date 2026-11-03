package consulo.typescript.documentation;

import com.intellij.javascript.documentation.JSDocumentationProvider;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.Language;
import consulo.language.ast.TokenSet;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.typescript.language.TypeScriptLanguage;
import consulo.typescript.language.psi.TypeScriptTypes;

/**
 * The documentation of TypeScript files - the same as JavaScript, documentation providers are not looked up by the base language.
 *
 * @since 2026-11-03
 */
@ExtensionImpl
public class TypeScriptDocumentationProvider extends JSDocumentationProvider {
    /**
     * The nodes wrapping a declaration - the documentation comment precedes the outermost of them
     */
    private static final TokenSet DECLARATION_WRAPPERS = TokenSet.create(
        TypeScriptTypes.STATEMENT,
        TypeScriptTypes.EXPORT_DECLARATION,
        TypeScriptTypes.AMBIENT_DECLARATION,
        TypeScriptTypes.EXPORT_DEFAULT_ASSIGNMENT
    );

    @Override
    @RequiredReadAction
    protected PsiElement getDocCommentAnchor(PsiElement element) {
        PsiElement anchor = element;
        PsiElement parent = anchor.getParent();
        while (parent != null && !(parent instanceof PsiFile) && DECLARATION_WRAPPERS.contains(parent.getNode().getElementType())) {
            anchor = parent;
            parent = anchor.getParent();
        }
        return anchor;
    }

    @Override
    public Language getLanguage() {
        return TypeScriptLanguage.INSTANCE;
    }
}

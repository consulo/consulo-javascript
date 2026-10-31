package consulo.typescript.language.impl.psi;

import com.intellij.lang.javascript.JSTokenTypes;
import consulo.annotation.access.RequiredReadAction;
import consulo.document.util.TextRange;
import consulo.javascript.module.resolve.JavaScriptModuleReference;
import consulo.javascript.module.resolve.JavaScriptModuleRequest;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiReference;
import org.jspecify.annotations.Nullable;

/**
 * The module specifier of {@code import ... from './x'} / {@code export ... from 'pkg'}: resolves like the import
 * itself ({@link TypeScriptModuleUtil#resolveModule}: the module resolvers, then the paths and packages of TypeScript).
 *
 * @since 2026-10-31
 */
public class TypeScriptModuleReference extends JavaScriptModuleReference {
    public TypeScriptModuleReference(PsiElement element, TextRange stringRange) {
        super(element, stringRange, JavaScriptModuleRequest.Kind.TYPES);
    }

    @Nullable
    @Override
    @RequiredReadAction
    public PsiElement resolve() {
        return TypeScriptModuleUtil.resolveModule(getElement(), getSpecifier());
    }

    /**
     * The reference of the specifier string among the children of an import or export declaration.
     */
    @RequiredReadAction
    public static PsiReference[] create(PsiElement declaration) {
        for (PsiElement child = declaration.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child.getNode().getElementType() == JSTokenTypes.STRING_LITERAL
                || child.getNode().getElementType() == JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL) {
                TextRange range = child.getTextRange().shiftLeft(declaration.getTextRange().getStartOffset());
                return range.getLength() <= 2 ? PsiReference.EMPTY_ARRAY : new PsiReference[]{new TypeScriptModuleReference(declaration, range)};
            }
        }
        return PsiReference.EMPTY_ARRAY;
    }
}

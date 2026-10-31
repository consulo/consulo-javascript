package consulo.typescript.language.impl.psi;

import com.intellij.lang.javascript.psi.JSElement;
import consulo.language.ast.IElementType;
import consulo.language.impl.psi.LazyParseablePsiElement;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import org.jspecify.annotations.Nullable;

/**
 * The content of {@code <script lang="ts">} (a lazily parsed chameleon): its statements are walked like the statements
 * of a file; seen from outside (a template of the same file) it declares its top-level declarations.
 *
 * @since 2026-10-31
 */
public class TypeScriptEmbeddedContentImpl extends LazyParseablePsiElement implements JSElement {
    public TypeScriptEmbeddedContentImpl(IElementType type, @Nullable CharSequence buffer) {
        super(type, buffer);
    }

    @Override
    public boolean processDeclarations(PsiScopeProcessor processor, ResolveState state, @Nullable PsiElement lastParent, PsiElement place) {
        if (lastParent != null) {
            // the statements before the place were walked as its siblings
            return true;
        }
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if ((child instanceof JSElement || child instanceof TypeScriptElementImpl) && !child.processDeclarations(processor, state, null, place)) {
                return false;
            }
        }
        return true;
    }

    @Override
    public String toString() {
        return "TypeScriptEmbeddedContent";
    }
}

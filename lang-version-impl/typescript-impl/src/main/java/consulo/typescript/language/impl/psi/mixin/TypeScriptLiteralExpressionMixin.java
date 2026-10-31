package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.JSTokenTypes;
import consulo.annotation.access.RequiredWriteAction;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.psi.LiteralTextEscaper;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiFileFactory;
import consulo.language.psi.PsiLanguageInjectionHost;
import consulo.language.psi.util.PsiTreeUtil;
import jakarta.annotation.Nonnull;

/**
 * A literal of TypeScript: a string hosts injected languages ({@code el.innerHTML = '<b>a</b>'}), like the literals of
 * JavaScript.
 *
 * @since 2026-10-31
 */
public class TypeScriptLiteralExpressionMixin extends TypeScriptExpressionMixin implements PsiLanguageInjectionHost {
    public TypeScriptLiteralExpressionMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public boolean isValidHost() {
        ASTNode token = getNode().getFirstChildNode();
        IElementType type = token == null ? null : token.getElementType();
        return type == JSTokenTypes.STRING_LITERAL || type == JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL;
    }

    @Override
    @RequiredWriteAction
    public PsiLanguageInjectionHost updateText(String text) {
        PsiFile containingFile = getContainingFile();
        PsiFile file = PsiFileFactory.getInstance(getProject())
            .createFileFromText(containingFile.getName(), containingFile.getLanguage(), getLanguageVersion(), text);
        TypeScriptLiteralExpressionMixin literal = PsiTreeUtil.findChildOfType(file, TypeScriptLiteralExpressionMixin.class);
        return literal == null ? this : (PsiLanguageInjectionHost) replace(literal);
    }

    @Override
    public LiteralTextEscaper<? extends PsiLanguageInjectionHost> createLiteralTextEscaper() {
        return LiteralTextEscaper.createSimple(this);
    }
}

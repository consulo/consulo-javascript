package consulo.typescript.language;

import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.ast.IElementTypeAsPsiFactory;
import consulo.language.psi.PsiElement;
import consulo.typescript.language.impl.psi.TypeScriptTypesFactory;
import org.jspecify.annotations.NonNull;

/**
 * @author VISTALL
 * @since 2026-03-17
 */
public class TypeScriptElementType extends IElementType implements IElementTypeAsPsiFactory {
    public TypeScriptElementType(@NonNull String debugName) {
        super(debugName, JavaScriptLanguage.INSTANCE);
    }

    @Override
    public PsiElement createElement(ASTNode astNode) {
        return TypeScriptTypesFactory.createElement(astNode);
    }
}

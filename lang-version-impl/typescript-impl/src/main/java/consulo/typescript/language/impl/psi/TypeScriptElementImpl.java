package consulo.typescript.language.impl.psi;

import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.psi.JavaScriptType;
import consulo.language.ast.ASTNode;
import consulo.language.impl.psi.ASTWrapperPsiElement;
import org.jspecify.annotations.NonNull;

/**
 * Base PSI element implementation for TypeScript.
 * Provides default getType() for JavaScriptTypeElement conformance in type expression subclasses.
 *
 * @author VISTALL
 * @since 2026-03-17
 */
public class TypeScriptElementImpl extends ASTWrapperPsiElement {
    public TypeScriptElementImpl(@NonNull ASTNode node) {
        super(node);
    }

    /**
     * Default getType() implementation that satisfies JavaScriptTypeElement and JSExpression contracts.
     * Override in specific mixins/implementations to provide actual type resolution.
     */
    @RequiredReadAction
    public JavaScriptType getType() {
        return null;
    }
}

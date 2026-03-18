package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.JSStatement;
import consulo.annotation.access.RequiredWriteAction;
import consulo.language.ast.ASTNode;
import consulo.language.util.IncorrectOperationException;
import consulo.typescript.language.impl.psi.TypeScriptElementImpl;
import consulo.typescript.language.psi.*;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Base mixin for all TypeScript statement PSI elements.
 * Provides default implementations for JSStatement methods and
 * default null getters for the declaration alternatives in TypeScriptStatement.
 */
public class TypeScriptStatementMixin extends TypeScriptElementImpl implements JSStatement {
    public TypeScriptStatementMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @RequiredWriteAction
    @Override
    public JSStatement addStatementBefore(JSStatement toAdd) throws IncorrectOperationException {
        return (JSStatement) getParent().addBefore(toAdd, this);
    }

    @RequiredWriteAction
    @Override
    public JSStatement addStatementAfter(JSStatement toAdd) throws IncorrectOperationException {
        return (JSStatement) getParent().addAfter(toAdd, this);
    }

    @RequiredWriteAction
    @Override
    public JSStatement replace(JSStatement with) {
        return (JSStatement) super.replace(with);
    }

    // --- Default null getters for declaration alternatives in TypeScriptStatement ---
    // These are overridden with real implementations in TypeScriptStatementImpl (generated).
    // Child statement Impls (if_statement, for_statement, etc.) inherit these defaults.

    @Nullable
    public TypeScriptAmbientDeclaration getAmbientDeclaration() {
        return null;
    }

    @Nullable
    public TypeScriptClassDeclaration getClassDeclaration() {
        return null;
    }

    @Nullable
    public TypeScriptEnumDeclaration getEnumDeclaration() {
        return null;
    }

    @Nullable
    public TypeScriptExportDeclaration getExportDeclaration() {
        return null;
    }

    @Nullable
    public TypeScriptFunctionDeclaration getFunctionDeclaration() {
        return null;
    }

    @Nullable
    public TypeScriptImportDeclaration getImportDeclaration() {
        return null;
    }

    @Nullable
    public TypeScriptInterfaceDeclaration getInterfaceDeclaration() {
        return null;
    }

    @Nullable
    public TypeScriptNamespaceDeclaration getNamespaceDeclaration() {
        return null;
    }

    @Nullable
    public TypeScriptTypeAliasDeclaration getTypeAliasDeclaration() {
        return null;
    }
}

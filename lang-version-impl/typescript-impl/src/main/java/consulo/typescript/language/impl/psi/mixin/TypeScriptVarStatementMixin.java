package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.stubs.JSVarStatementStub;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.language.ast.ASTNode;
import consulo.language.impl.psi.stub.StubBasedPsiElementBase;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.psi.stub.IStubElementType;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.typescript.language.psi.*;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * Mixin for TypeScript var/let/const statements.
 * Extends StubBasedPsiElementBase to satisfy JSVarStatement's StubBasedPsiElement contract.
 * Implements JSStatement methods directly (can't extend TypeScriptStatementMixin due to single inheritance).
 *
 * @author VISTALL
 * @since 2026-03-17
 */
// T only lets Grammar-Kit generate the stub constructor of the impl (it needs a generic super class)
public class TypeScriptVarStatementMixin<T extends JSVarStatementStub> extends StubBasedPsiElementBase<JSVarStatementStub> implements JSVarStatement {
    public TypeScriptVarStatementMixin(@Nonnull ASTNode node) {
        super(node);
    }

    public TypeScriptVarStatementMixin(@Nonnull JSVarStatementStub stub, @Nonnull IStubElementType type) {
        super(stub, type);
    }


    @Override
    public String toString() {
        return getClass().getSimpleName() + ":" + getElementType();
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSVarStatement(this);
        }
        else {
            visitor.visitElement(this);
        }
    }

    // --- JSVarStatement ---

    @RequiredReadAction
    @Override
    public JSVariable[] getVariables() {
        JSVariable[] vars = PsiTreeUtil.getChildrenOfType(this, JSVariable.class);
        return vars != null ? vars : JSVariable.EMPTY_ARRAY;
    }

    @Nullable
    @RequiredReadAction
    @Override
    public JSDestructuringElement getDestructuringElement() {
        return null;
    }

    @Override
    public void declareVariable(String name, JSExpression initializer) {
        throw new UnsupportedOperationException("Not yet implemented");
    }

    // --- JSStatement ---

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

    // --- Default null getters for TypeScriptStatement declaration alternatives ---
    // (Needed because var_statement extends statement and inherits these from generated interface)

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
    public TypeScriptExportDefaultAssignment getExportDefaultAssignment() {
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

    // JavaScript's JSVarStatementImpl
    @Override
    @RequiredReadAction
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState state,
        @Nullable PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        for (JSVariable variable : getVariables()) {
            if (lastParent != null && lastParent.getParent() == variable) {
                break;
            }
            if (!variable.processDeclarations(processor, state, null, place)) {
                return false;
            }
        }
        return true;
    }
}

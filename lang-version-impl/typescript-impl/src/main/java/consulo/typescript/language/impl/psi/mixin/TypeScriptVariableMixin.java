package consulo.typescript.language.impl.psi.mixin;

import consulo.language.impl.psi.stub.StubBasedPsiElementBase;
import com.intellij.lang.javascript.psi.stubs.JSVariableStub;
import consulo.language.psi.stub.IStubElementType;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.psi.JavaScriptType;
import consulo.javascript.psi.JSDeclaredTypeNameProvider;
import consulo.javascript.psi.JSConstructedTypeProvider;
import consulo.javascript.language.psi.JavaScriptTypeElement;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.util.IncorrectOperationException;
import consulo.typescript.language.impl.psi.TypeScriptElementImpl;
import consulo.typescript.language.impl.psi.TypeScriptResolveUtil;
import consulo.typescript.language.psi.TypeScriptTypes;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * Mixin for TypeScript variable declarations.
 * Implements JSVariable to integrate with JavaScript PSI infrastructure.
 *
 * @author VISTALL
 * @since 2026-03-17
 */
public class TypeScriptVariableMixin<T extends JSVariableStub> extends StubBasedPsiElementBase<T> implements JSVariable, JSConstructedTypeProvider, JSDeclaredTypeNameProvider {
    public TypeScriptVariableMixin(@Nonnull ASTNode node) {
        super(node);
    }

    public TypeScriptVariableMixin(@Nonnull T stub, @Nonnull IStubElementType type) {
        super(stub, type);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSVariable(this);
        }
        else {
            super.accept(visitor);
        }
    }

    // --- JSVariable ---

    @Override
    public boolean hasInitializer() {
        T stub = getGreenStub();
        return stub != null ? stub.getInitializerText() != null : getInitializer() != null;
    }

    /**
     * The stub first: the JavaScript type evaluation asks the initializer of every variable it meets, a declaration
     * without one ({@code declare var Date: DateConstructor} of lib.es5.d.ts) doesn't load the AST of its file.
     */
    @RequiredReadAction
    @Nullable
    @Override
    public JSExpression getInitializer() {
        T stub = getGreenStub();
        if (stub != null && stub.getInitializerText() == null) {
            return null;
        }
        ASTNode eq = getNode().findChildByType(JSTokenTypes.EQ);
        if (eq != null) {
            for (PsiElement child = eq.getPsi().getNextSibling(); child != null; child = child.getNextSibling()) {
                if (child instanceof JSExpression expr) {
                    return expr;
                }
            }
        }
        return null;
    }

    @RequiredReadAction
    @Override
    public String getInitializerText() {
        T stub = getGreenStub();
        if (stub != null) {
            return stub.getInitializerText();
        }
        JSExpression init = getInitializer();
        return init != null ? init.getText() : null;
    }

    @Override
    public void setInitializer(JSExpression expr) throws IncorrectOperationException {
        throw new IncorrectOperationException("Not yet implemented");
    }

    @RequiredReadAction
    @Override
    public JavaScriptType getType() {
        return JavaScriptType.UNKNOWN;
    }

    @Nullable
    @Override
    @RequiredReadAction
    public String getConstructedTypeName() {
        JSClass constructed = TypeScriptResolveUtil.findConstructedClass(this);
        return constructed != null ? constructed.getQualifiedName() : null;
    }

    /**
     * The class name of the declared type: {@code HTMLElement | null} is {@code HTMLElement}.
     */
    @Nullable
    @Override
    @RequiredReadAction
    public String getDeclaredTypeName() {
        return TypeScriptResolveUtil.getTypeName(getTypeString());
    }

    /**
     * The text of the type annotation (the type text of the stub).
     */
    @Deprecated
    @Nullable
    @Override
    @RequiredReadAction
    public String getTypeString() {
        T stub = getGreenStub();
        return stub != null ? stub.getTypeString() : TypeScriptResolveUtil.getTypeAnnotationText(this);
    }

    @Nullable
    @RequiredReadAction
    @Override
    public JavaScriptTypeElement getTypeElement() {
        return null;
    }

    @Override
    public boolean isConst() {
        return false;
    }

    @Override
    public boolean isLocal() {
        return false;
    }

    @Override
    public boolean isDeprecated() {
        return false;
    }

    // --- JSQualifiedNamedElement ---

    @RequiredReadAction
    @Override
    public String getQualifiedName() {
        var stub = getGreenStub();
        return stub != null ? stub.getQualifiedName() : TypeScriptResolveUtil.qualifyByNamespaces(this, getName());
    }

    // --- JSNamedElement / PsiNamedElement / PsiNameIdentifierOwner ---

    @RequiredReadAction
    @Override
    public String getName() {
        var stub = getGreenStub();
        if (stub != null) {
            return stub.getName();
        }
        PsiElement id = getNameIdentifier();
        return id != null ? id.getText() : null;
    }

    @Override
    @RequiredReadAction
    public PsiElement setName(@Nonnull String name) throws IncorrectOperationException {
        TypeScriptResolveUtil.rename(getNameIdentifier(), name);
        return this;
    }

    @RequiredReadAction
    @Nullable
    @Override
    public PsiElement getNameIdentifier() {
        return TypeScriptResolveUtil.findNameIdentifier(this);
    }

    // --- JSAttributeListOwner ---

    @Nullable
    @Override
    public JSAttributeList getAttributeList() {
        return null;
    }

    // JavaScript's JSVariableBaseImpl
    @Override
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState state,
        @Nullable PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        if (lastParent != null && lastParent.getParent() == this) {
            processor.handleEvent(PsiScopeProcessor.Event.SET_DECLARATION_HOLDER, this);
        }
        // a class property is resolved through its qualifier (this.x), it isn't in scope of the other members
        if (getNode().getElementType() == TypeScriptTypes.PROPERTY_DECLARATION) {
            return true;
        }
        // const {a, b: c} = ...: the names of the pattern
        ASTNode bindingPattern = getNode().findChildByType(TypeScriptTypes.BINDING_PATTERN);
        if (bindingPattern != null && bindingPattern.getPsi() instanceof TypeScriptBindingPatternMixin pattern && !pattern.isName()) {
            return pattern.processNames(processor, state);
        }
        return processor.execute(this, state);
    }

    @Override
    public String toString() {
        return getClass().getSimpleName() + "(" + getElementType() + ")";
    }
}

package consulo.typescript.language.impl.stub;

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSVarStatement;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.stubs.JSClassStub;
import com.intellij.lang.javascript.psi.stubs.JSFunctionStub;
import com.intellij.lang.javascript.psi.stubs.JSVarStatementStub;
import com.intellij.lang.javascript.psi.stubs.JSVariableStub;
import com.intellij.lang.javascript.types.JSClassElementType;
import com.intellij.lang.javascript.types.JSFunctionElementType;
import com.intellij.lang.javascript.types.JSVarStatementElementType;
import com.intellij.lang.javascript.types.JSVariableElementType;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.ast.IFileElementType;
import consulo.language.psi.PsiElement;
import consulo.language.psi.stub.IStubElementType;
import consulo.typescript.language.TypeScriptLanguage;
import consulo.typescript.language.impl.psi.*;
import consulo.typescript.language.psi.TypeScriptTypes;

/**
 * Stub element types of the TypeScript declarations. They reuse the JavaScript stubs, serialization and indexing
 * (JavaScriptIndexKeys), so the JavaScript resolve and indexes see TypeScript declarations; they only
 * create TypeScript PSI. Referenced from TypeScript.bnf through {@link #factory}.
 *
 * Field names = debug names (see {@link TypeScriptStubElementHolder}).
 */
public interface TypeScriptStubElementTypes {
    IStubElementType<JSClassStub, JSClass> CLASS_DECLARATION = new JSClassElementType("CLASS_DECLARATION", TypeScriptLanguage.INSTANCE) {
        @Override
        public PsiElement createElement(ASTNode node) {
            return new TypeScriptClassDeclarationImpl(node);
        }

        @Override
        public JSClass createPsi(JSClassStub stub) {
            return new TypeScriptClassDeclarationImpl(stub, this);
        }

        @Override
        public String getExternalId() {
            return TypeScriptStubElementHolder.PREFIX + this;
        }

        @Override
        public String toString() {
            return getDebugName();
        }
    };

    IStubElementType<JSClassStub, JSClass> INTERFACE_DECLARATION = new JSClassElementType("INTERFACE_DECLARATION", TypeScriptLanguage.INSTANCE) {
        @Override
        public PsiElement createElement(ASTNode node) {
            return new TypeScriptInterfaceDeclarationImpl(node);
        }

        @Override
        public JSClass createPsi(JSClassStub stub) {
            return new TypeScriptInterfaceDeclarationImpl(stub, this);
        }

        @Override
        public String getExternalId() {
            return TypeScriptStubElementHolder.PREFIX + this;
        }

        @Override
        public String toString() {
            return getDebugName();
        }
    };

    IStubElementType<JSFunctionStub, JSFunction> FUNCTION_DECLARATION = new TypeScriptFunctionElementType("FUNCTION_DECLARATION") {
        @Override
        public PsiElement createElement(ASTNode node) {
            return new TypeScriptFunctionDeclarationImpl(node);
        }

        @Override
        public JSFunction createPsi(JSFunctionStub stub) {
            return new TypeScriptFunctionDeclarationImpl(stub, this);
        }
    };

    IStubElementType<JSFunctionStub, JSFunction> METHOD_DECLARATION = new TypeScriptFunctionElementType("METHOD_DECLARATION") {
        @Override
        public PsiElement createElement(ASTNode node) {
            return new TypeScriptMethodDeclarationImpl(node);
        }

        @Override
        public JSFunction createPsi(JSFunctionStub stub) {
            return new TypeScriptMethodDeclarationImpl(stub, this);
        }
    };

    IStubElementType<JSFunctionStub, JSFunction> CONSTRUCTOR_DECLARATION = new TypeScriptFunctionElementType("CONSTRUCTOR_DECLARATION") {
        @Override
        public PsiElement createElement(ASTNode node) {
            return new TypeScriptConstructorDeclarationImpl(node);
        }

        @Override
        public JSFunction createPsi(JSFunctionStub stub) {
            return new TypeScriptConstructorDeclarationImpl(stub, this);
        }
    };

    IStubElementType<JSFunctionStub, JSFunction> ACCESSOR_DECLARATION = new TypeScriptFunctionElementType("ACCESSOR_DECLARATION") {
        @Override
        public PsiElement createElement(ASTNode node) {
            return new TypeScriptAccessorDeclarationImpl(node);
        }

        @Override
        public JSFunction createPsi(JSFunctionStub stub) {
            return new TypeScriptAccessorDeclarationImpl(stub, this);
        }
    };

    IStubElementType<JSFunctionStub, JSFunction> METHOD_SIGNATURE_MEMBER = new TypeScriptFunctionElementType("METHOD_SIGNATURE_MEMBER") {
        @Override
        public PsiElement createElement(ASTNode node) {
            return new TypeScriptMethodSignatureMemberImpl(node);
        }

        @Override
        public JSFunction createPsi(JSFunctionStub stub) {
            return new TypeScriptMethodSignatureMemberImpl(stub, this);
        }
    };

    IStubElementType<JSVarStatementStub, JSVarStatement> VAR_STATEMENT = new JSVarStatementElementType("VAR_STATEMENT", TypeScriptLanguage.INSTANCE) {
        @Override
        public boolean shouldCreateStub(ASTNode node) {
            return isTopLevel(node);
        }

        @Override
        public PsiElement createElement(ASTNode node) {
            return new TypeScriptVarStatementImpl(node);
        }

        @Override
        public JSVarStatement createPsi(JSVarStatementStub stub) {
            return new TypeScriptVarStatementImpl(stub, this);
        }

        @Override
        public String getExternalId() {
            return TypeScriptStubElementHolder.PREFIX + this;
        }

        @Override
        public String toString() {
            return getDebugName();
        }
    };

    IStubElementType<JSVariableStub, JSVariable> VARIABLE_DECLARATION = new TypeScriptVariableElementType("VARIABLE_DECLARATION") {
        @Override
        public boolean shouldCreateStub(ASTNode node) {
            ASTNode parent = node.getTreeParent();
            return parent != null && parent.getElementType() == VAR_STATEMENT && isTopLevel(parent);
        }

        @Override
        public PsiElement createElement(ASTNode node) {
            return new TypeScriptVariableDeclarationImpl(node);
        }

        @Override
        public JSVariable createPsi(JSVariableStub stub) {
            return new TypeScriptVariableDeclarationImpl(stub, this);
        }
    };

    /**
     * Class properties: stubs (members of class stubs) but not global names.
     */
    IStubElementType<JSVariableStub, JSVariable> PROPERTY_DECLARATION = new TypeScriptVariableElementType("PROPERTY_DECLARATION") {
        @Override
        public boolean shouldCreateStub(ASTNode node) {
            return true;
        }

        @Override
        protected boolean doIndexName(JSVariableStub stub, String name, String fqn) {
            return false;
        }

        @Override
        public PsiElement createElement(ASTNode node) {
            return new TypeScriptPropertyDeclarationImpl(node);
        }

        @Override
        public JSVariable createPsi(JSVariableStub stub) {
            return new TypeScriptPropertyDeclarationImpl(stub, this);
        }
    };

    /**
     * Parameters: a stub element type (their PSI is stub-based like the one of variables) that never creates stubs.
     */
    IStubElementType<JSVariableStub, JSVariable> PARAMETER = new TypeScriptVariableElementType("PARAMETER") {
        @Override
        public boolean shouldCreateStub(ASTNode node) {
            return false;
        }

        @Override
        public PsiElement createElement(ASTNode node) {
            return new TypeScriptParameterImpl(node);
        }

        @Override
        public JSVariable createPsi(JSVariableStub stub) {
            throw new UnsupportedOperationException("parameters have no stubs");
        }
    };

    /**
     * A statement of the file itself, maybe wrapped by {@code export}, {@code declare} or the generic statement node.
     */
    static boolean isTopLevel(ASTNode node) {
        ASTNode parent = node.getTreeParent();
        while (parent != null
            && (parent.getElementType() == TypeScriptTypes.STATEMENT
            || parent.getElementType() == TypeScriptTypes.EXPORT_DECLARATION
            || parent.getElementType() == TypeScriptTypes.AMBIENT_DECLARATION
            || parent.getElementType() == TypeScriptTypes.EXPORT_DEFAULT_ASSIGNMENT)) {
            parent = parent.getTreeParent();
        }
        return parent != null && parent.getElementType() instanceof IFileElementType;
    }

    /**
     * Grammar-Kit {@code elementTypeFactory} of TypeScript.bnf.
     */
    static IElementType factory(String name) {
        return switch (name) {
            case "CLASS_DECLARATION" -> CLASS_DECLARATION;
            case "INTERFACE_DECLARATION" -> INTERFACE_DECLARATION;
            case "FUNCTION_DECLARATION" -> FUNCTION_DECLARATION;
            case "METHOD_DECLARATION" -> METHOD_DECLARATION;
            case "CONSTRUCTOR_DECLARATION" -> CONSTRUCTOR_DECLARATION;
            case "ACCESSOR_DECLARATION" -> ACCESSOR_DECLARATION;
            case "METHOD_SIGNATURE_MEMBER" -> METHOD_SIGNATURE_MEMBER;
            case "VAR_STATEMENT" -> VAR_STATEMENT;
            case "VARIABLE_DECLARATION" -> VARIABLE_DECLARATION;
            case "PROPERTY_DECLARATION" -> PROPERTY_DECLARATION;
            case "PARAMETER" -> PARAMETER;
            default -> throw new IllegalArgumentException("no stub element type " + name);
        };
    }

    abstract class TypeScriptFunctionElementType extends JSFunctionElementType {
        TypeScriptFunctionElementType(String debugName) {
            super(debugName, TypeScriptLanguage.INSTANCE);
        }

        @Override
        public String getExternalId() {
            return TypeScriptStubElementHolder.PREFIX + this;
        }

        @Override
        public String toString() {
            return getDebugName();
        }
    }

    abstract class TypeScriptVariableElementType extends JSVariableElementType {
        TypeScriptVariableElementType(String debugName) {
            super(debugName, TypeScriptLanguage.INSTANCE);
        }

        @Override
        public String getExternalId() {
            return TypeScriptStubElementHolder.PREFIX + this;
        }

        @Override
        public String toString() {
            return getDebugName();
        }
    }
}

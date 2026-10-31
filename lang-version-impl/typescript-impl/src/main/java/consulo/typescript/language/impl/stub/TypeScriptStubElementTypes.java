package consulo.typescript.language.impl.stub;

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSReferenceList;
import com.intellij.lang.javascript.psi.JSVarStatement;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.stubs.JSClassStub;
import com.intellij.lang.javascript.psi.stubs.JSFunctionStub;
import com.intellij.lang.javascript.psi.stubs.JSReferenceListStub;
import com.intellij.lang.javascript.psi.stubs.JSVarStatementStub;
import com.intellij.lang.javascript.psi.stubs.JSVariableStub;
import com.intellij.lang.javascript.types.JSClassElementType;
import com.intellij.lang.javascript.types.JSFunctionElementType;
import com.intellij.lang.javascript.types.JSReferenceListElementType;
import com.intellij.lang.javascript.types.JSVarStatementElementType;
import com.intellij.lang.javascript.types.JSVariableElementType;
import consulo.javascript.language.psi.stub.JavaScriptIndexKeys;
import com.intellij.lang.javascript.psi.JSQualifiedNamedElement;
import consulo.javascript.types.JSQualifiedStubElementType;
import consulo.annotation.access.RequiredReadAction;
import consulo.index.io.StringRef;
import consulo.language.psi.stub.StubInputStream;
import consulo.language.psi.stub.StubOutputStream;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.ast.IFileElementType;
import consulo.language.psi.PsiElement;
import consulo.language.psi.stub.IStubElementType;
import consulo.language.psi.stub.IndexSink;
import consulo.language.psi.stub.StubElement;
import consulo.typescript.language.TypeScriptLanguage;
import consulo.typescript.language.impl.psi.*;
import consulo.typescript.language.psi.TypeScriptTypes;

import java.io.IOException;

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
        public void indexStub(JSClassStub stub, IndexSink sink) {
            // a member of a namespace isn't a global name: only its qualified name (Intl.Collator) is indexed
            if (!isNamespaceMember(stub)) {
                super.indexStub(stub, sink);
            }
            else if (stub.getQualifiedName() != null) {
                sink.occurrence(JavaScriptIndexKeys.ELEMENTS_BY_QNAME, stub.getQualifiedName());
            }
        }

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
        public void indexStub(JSClassStub stub, IndexSink sink) {
            // a member of a namespace isn't a global name: only its qualified name (Intl.Collator) is indexed
            if (!isNamespaceMember(stub)) {
                super.indexStub(stub, sink);
            }
            else if (stub.getQualifiedName() != null) {
                sink.occurrence(JavaScriptIndexKeys.ELEMENTS_BY_QNAME, stub.getQualifiedName());
            }
        }

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

    /**
     * {@code namespace Intl { ... }} of the file (or of a namespace, not indexed then): a global name with members, like
     * a class. Not {@code declare module "x"} (a module of its own).
     */
    IStubElementType<JSClassStub, JSClass> NAMESPACE_DECLARATION = new JSClassElementType("NAMESPACE_DECLARATION", TypeScriptLanguage.INSTANCE) {
        @Override
        public boolean shouldCreateStub(ASTNode node) {
            return node.findChildByType(TypeScriptTypes.QUALIFIED_NAME) != null && (isTopLevel(node) || isNamespaceMember(node));
        }

        @Override
        public void indexStub(JSClassStub stub, IndexSink sink) {
            if (!isNamespaceMember(stub)) {
                super.indexStub(stub, sink);
            }
            else if (stub.getQualifiedName() != null) {
                sink.occurrence(JavaScriptIndexKeys.ELEMENTS_BY_QNAME, stub.getQualifiedName());
            }
        }

        @Override
        public PsiElement createElement(ASTNode node) {
            return new TypeScriptNamespaceDeclarationImpl(node);
        }

        @Override
        public JSClass createPsi(JSClassStub stub) {
            return new TypeScriptNamespaceDeclarationImpl(stub, this);
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

    /**
     * {@code type Record<K, V> = ...} of the file or of a namespace (only its qualified name is indexed then): a type
     * only ({@link consulo.javascript.psi.JSTypeOnlyDeclaration}).
     */
    IStubElementType<TypeScriptTypeAliasStub, JSQualifiedNamedElement> TYPE_ALIAS_DECLARATION =
        new JSQualifiedStubElementType<TypeScriptTypeAliasStub, JSQualifiedNamedElement>("TYPE_ALIAS_DECLARATION", TypeScriptLanguage.INSTANCE) {
            @Override
            public boolean shouldCreateStub(ASTNode node) {
                return isTopLevel(node) || isNamespaceMember(node);
            }

            @Override
            protected boolean doIndexName(TypeScriptTypeAliasStub stub, String name, String fqn) {
                return !isNamespaceMember(stub);
            }

            @Override
            @RequiredReadAction
            public TypeScriptTypeAliasStub createStub(JSQualifiedNamedElement psi, StubElement parentStub) {
                return new TypeScriptTypeAliasStub(StringRef.fromNullableString(psi.getName()),
                    StringRef.fromNullableString(psi.getQualifiedName()), parentStub, this);
            }

            @Override
            public void serialize(TypeScriptTypeAliasStub stub, StubOutputStream dataStream) throws IOException {
                dataStream.writeName(stub.getName());
                dataStream.writeName(stub.getQualifiedName());
            }

            @Override
            public TypeScriptTypeAliasStub deserialize(StubInputStream dataStream, StubElement parentStub) throws IOException {
                return new TypeScriptTypeAliasStub(dataStream.readName(), dataStream.readName(), parentStub, this);
            }

            @Override
            public PsiElement createElement(ASTNode node) {
                return new TypeScriptTypeAliasDeclarationImpl(node);
            }

            @Override
            public JSQualifiedNamedElement createPsi(TypeScriptTypeAliasStub stub) {
                return new TypeScriptTypeAliasDeclarationImpl(stub, this);
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
            return isTopLevel(node) || isNamespaceMember(node);
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
            return parent != null && parent.getElementType() == VAR_STATEMENT && VAR_STATEMENT.shouldCreateStub(parent);
        }

        @Override
        protected boolean doIndexName(JSVariableStub stub, String name, String fqn) {
            // Intl.Collator of `declare namespace Intl { var Collator }` isn't a global name
            StubElement<?> varStatement = stub.getParentStub();
            return varStatement == null || !isNamespaceMember(varStatement);
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
     * The property signatures of an interface: stubbed members (the global resolve doesn't parse lib.dom.d.ts for
     * {@code document.title}), not global names. AST only in an inline object type.
     */
    IStubElementType<JSVariableStub, JSVariable> PROPERTY_SIGNATURE_MEMBER = new TypeScriptVariableElementType("PROPERTY_SIGNATURE_MEMBER") {
        @Override
        public boolean shouldCreateStub(ASTNode node) {
            return isInterfaceMember(node);
        }

        @Override
        protected boolean doIndexName(JSVariableStub stub, String name, String fqn) {
            return false;
        }

        @Override
        public PsiElement createElement(ASTNode node) {
            return new TypeScriptPropertySignatureMemberImpl(node);
        }

        @Override
        public JSVariable createPsi(JSVariableStub stub) {
            return new TypeScriptPropertySignatureMemberImpl(stub, this);
        }
    };

    /**
     * {@code new (): Date} of an interface ({@code DateConstructor}) or of the inline type of a global variable
     * ({@code declare var Image: { new (): HTMLImageElement }}): the stub keeps the return type text, so
     * {@code new Image()} doesn't parse the declaring file. Never a global name.
     */
    IStubElementType<JSFunctionStub, JSFunction> CONSTRUCT_SIGNATURE_MEMBER = new TypeScriptFunctionElementType("CONSTRUCT_SIGNATURE_MEMBER") {
        @Override
        public boolean shouldCreateStub(ASTNode node) {
            if (isInterfaceMember(node)) {
                return true;
            }
            // object type < type annotation < stubbed variable declaration
            ASTNode objectType = node.getTreeParent();
            ASTNode annotation = objectType == null ? null : objectType.getTreeParent();
            ASTNode variable = annotation == null ? null : annotation.getTreeParent();
            return variable != null
                && objectType.getElementType() == TypeScriptTypes.OBJECT_TYPE
                && annotation.getElementType() == TypeScriptTypes.TYPE_ANNOTATION
                && variable.getElementType() == VARIABLE_DECLARATION
                && VARIABLE_DECLARATION.shouldCreateStub(variable);
        }

        @Override
        public PsiElement createElement(ASTNode node) {
            return new TypeScriptConstructSignatureMemberImpl(node);
        }

        @Override
        public JSFunction createPsi(JSFunctionStub stub) {
            return new TypeScriptConstructSignatureMemberImpl(stub, this);
        }
    };

    /**
     * {@code interface A extends B, C}: the stub keeps the type names, so the inherited members of a declaration file
     * resolve without its AST.
     */
    IStubElementType<JSReferenceListStub, JSReferenceList> EXTENDS_LIST = new JSReferenceListElementType("EXTENDS_LIST", TypeScriptLanguage.INSTANCE) {
        @Override
        public boolean shouldCreateStub(ASTNode node) {
            ASTNode parent = node.getTreeParent();
            return parent != null && parent.getElementType() == INTERFACE_DECLARATION;
        }

        @Override
        public PsiElement createElement(ASTNode node) {
            return new TypeScriptExtendsListImpl(node);
        }

        @Override
        public JSReferenceList createPsi(JSReferenceListStub stub) {
            return new TypeScriptExtendsListImpl(stub, this);
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

    /**
     * A declaration of the body of a (stubbed) namespace, maybe wrapped by {@code export}, {@code declare} or the
     * statement node.
     */
    static boolean isNamespaceMember(ASTNode node) {
        ASTNode parent = node.getTreeParent();
        while (parent != null
            && (parent.getElementType() == TypeScriptTypes.STATEMENT
            || parent.getElementType() == TypeScriptTypes.EXPORT_DECLARATION
            || parent.getElementType() == TypeScriptTypes.AMBIENT_DECLARATION)) {
            parent = parent.getTreeParent();
        }
        ASTNode namespace = parent != null && parent.getElementType() == TypeScriptTypes.BLOCK_STATEMENT ? parent.getTreeParent() : null;
        return namespace != null && namespace.getElementType() == NAMESPACE_DECLARATION && NAMESPACE_DECLARATION.shouldCreateStub(namespace);
    }

    /**
     * A stub of a namespace's body.
     */
    /**
     * A member of a class or interface ({@code innerHTML} of {@code interface Element}), not of a namespace: found by
     * name for a value of unknown type.
     */
    static boolean isClassOrInterfaceMember(StubElement<?> stub) {
        StubElement<?> parent = stub.getParentStub();
        IStubElementType<?, ?> type = parent == null ? null : parent.getStubType();
        return type == INTERFACE_DECLARATION || type == CLASS_DECLARATION;
    }

    static boolean isNamespaceMember(StubElement<?> stub) {
        StubElement<?> parent = stub.getParentStub();
        return parent != null && parent.getStubType() == NAMESPACE_DECLARATION;
    }

    private static boolean isInterfaceMember(ASTNode node) {
        ASTNode parent = node.getTreeParent();
        return parent != null && parent.getElementType() == INTERFACE_DECLARATION;
    }

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
     * A statement of the file itself, maybe wrapped by {@code export}, {@code declare} or the generic statement node, or
     * of a {@code declare global { ... }} block (global declarations of a module).
     */
    static boolean isTopLevel(ASTNode node) {
        ASTNode parent = node.getTreeParent();
        while (parent != null
            && (parent.getElementType() == TypeScriptTypes.STATEMENT
            || parent.getElementType() == TypeScriptTypes.EXPORT_DECLARATION
            || parent.getElementType() == TypeScriptTypes.AMBIENT_DECLARATION
            || parent.getElementType() == TypeScriptTypes.EXPORT_DEFAULT_ASSIGNMENT
            || isGlobalAugmentationBlock(parent))) {
            parent = parent.getTreeParent();
        }
        return parent != null && parent.getElementType() instanceof IFileElementType;
    }

    /**
     * The block of {@code declare global { ... }}.
     */
    static boolean isGlobalAugmentationBlock(ASTNode node) {
        ASTNode parent = node.getTreeParent();
        return node.getElementType() == TypeScriptTypes.BLOCK_STATEMENT && parent != null
            && parent.getElementType() == TypeScriptTypes.AMBIENT_DECLARATION && parent.findChildByType(TypeScriptTypes.GLOBAL_KEYWORD) != null;
    }

    /**
     * Grammar-Kit {@code elementTypeFactory} of TypeScript.bnf.
     */
    static IElementType factory(String name) {
        return switch (name) {
            case "CLASS_DECLARATION" -> CLASS_DECLARATION;
            case "INTERFACE_DECLARATION" -> INTERFACE_DECLARATION;
            case "NAMESPACE_DECLARATION" -> NAMESPACE_DECLARATION;
            case "TYPE_ALIAS_DECLARATION" -> TYPE_ALIAS_DECLARATION;
            case "FUNCTION_DECLARATION" -> FUNCTION_DECLARATION;
            case "METHOD_DECLARATION" -> METHOD_DECLARATION;
            case "CONSTRUCTOR_DECLARATION" -> CONSTRUCTOR_DECLARATION;
            case "ACCESSOR_DECLARATION" -> ACCESSOR_DECLARATION;
            case "METHOD_SIGNATURE_MEMBER" -> METHOD_SIGNATURE_MEMBER;
            case "VAR_STATEMENT" -> VAR_STATEMENT;
            case "VARIABLE_DECLARATION" -> VARIABLE_DECLARATION;
            case "PROPERTY_DECLARATION" -> PROPERTY_DECLARATION;
            case "PARAMETER" -> PARAMETER;
            case "PROPERTY_SIGNATURE_MEMBER" -> PROPERTY_SIGNATURE_MEMBER;
            case "CONSTRUCT_SIGNATURE_MEMBER" -> CONSTRUCT_SIGNATURE_MEMBER;
            case "EXTENDS_LIST" -> EXTENDS_LIST;
            default -> throw new IllegalArgumentException("no stub element type " + name);
        };
    }

    abstract class TypeScriptFunctionElementType extends JSFunctionElementType {
        TypeScriptFunctionElementType(String debugName) {
            super(debugName, TypeScriptLanguage.INSTANCE);
        }

        @Override
        protected boolean doIndexMemberName(JSFunctionStub stub, String name) {
            return isClassOrInterfaceMember(stub);
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
        protected boolean doIndexMemberName(JSVariableStub stub, String name) {
            return isClassOrInterfaceMember(stub);
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

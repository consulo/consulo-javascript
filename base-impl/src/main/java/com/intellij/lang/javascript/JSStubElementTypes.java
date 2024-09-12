package com.intellij.lang.javascript;

import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSFunctionImpl;
import com.intellij.lang.javascript.psi.stubs.*;
import com.intellij.lang.javascript.types.*;
import consulo.javascript.impl.language.psi.JSStubElementType;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;

import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 29-Aug-22
 */
public interface JSStubElementTypes {
    JSStubElementType<JSFunctionStub, JSFunction> FUNCTION_DECLARATION = new JSFunctionElementType("FUNCTION_DECLARATION") {
        @Override
        public JSFunction createPsi(@Nonnull JSFunctionStub stub) {
            return new JSFunctionImpl(stub, this);
        }

        @Nonnull
        @Override
        public PsiElement createElement(@Nonnull ASTNode astNode) {
            return new JSFunctionImpl(astNode);
        }
    };
    JSStubElementType<JSParameterListStub, JSParameterList> PARAMETER_LIST = new JSParameterListElementType();

    JSStubElementType<JSParameterStub, JSParameter> FORMAL_PARAMETER = new JSParameterElementType();

    JSStubElementType<JSVariableStub, JSVariable> VARIABLE = new JSVariableElementType();

    JSStubElementType<JSAttributeStub, JSAttribute> ATTRIBUTE = new JSAttributeElementType();
    JSStubElementType<JSAttributeNameValuePairStub, JSAttributeNameValuePair> ATTRIBUTE_NAME_VALUE_PAIR =
        new JSAttributeNameValuePairType();
    JSStubElementType<JSAttributeListStub, JSAttributeList> ATTRIBUTE_LIST = new JSAttributeListElementType();
    JSStubElementType<JSPackageStatementStub, JSPackageStatement> PACKAGE_STATEMENT = new JSPackageStatementElementType();

    JSStubElementType<JSClassStub, JSClass> CLASS = new JSClassElementType();
    JSStubElementType<JSReferenceListStub, JSReferenceList> EXTENDS_LIST = new JSReferenceListElementType("EXTENDS_LIST");
    JSStubElementType<JSReferenceListStub, JSReferenceList> IMPLEMENTS_LIST = new JSReferenceListElementType("IMPLEMENTS_LIST");
    JSStubElementType<JSUseNamespaceDirectiveStub, JSUseNamespaceDirective> USE_NAMESPACE_DIRECTIVE = new JSUseNamespaceDirectiveType();
    JSStubElementType<JSIncludeDirectiveStub, JSIncludeDirective> INCLUDE_DIRECTIVE = new JSIncludeDirectiveElementType();
    JSStubElementType<JSNamespaceDeclarationStub, JSNamespaceDeclaration> NAMESPACE_DECLARATION = new JSNamespaceDeclarationElementType();

    JSStubElementType<JSVarStatementStub, JSVarStatement> VAR_STATEMENT = new JSVarStatementElementType();
}

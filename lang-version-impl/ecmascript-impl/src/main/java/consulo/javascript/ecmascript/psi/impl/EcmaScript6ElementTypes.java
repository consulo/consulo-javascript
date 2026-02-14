package consulo.javascript.ecmascript.psi.impl;

import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.ast.ElementTypeAsPsiFactory;
import consulo.language.ast.IElementType;

/**
 * @author VISTALL
 * @since 2019-12-14
 */
public interface EcmaScript6ElementTypes {
    IElementType EXPORT_DEFAULT_ASSIGMENT =
        new ElementTypeAsPsiFactory("EXPORT_DEFAULT_ASSIGMENT", JavaScriptLanguage.INSTANCE, ES6ExportDefaultAssignmentImpl::new);

    IElementType IMPORT_DECLARATION =
        new ElementTypeAsPsiFactory("IMPORT_DECLARATION", JavaScriptLanguage.INSTANCE, ES6ImportDeclarationImpl::new);

    IElementType IMPORTED_BINDING =
        new ElementTypeAsPsiFactory("IMPORTED_BINDING", JavaScriptLanguage.INSTANCE, ES6ImportedBindingImpl::new);

    IElementType NAMED_IMPORTS =
        new ElementTypeAsPsiFactory("NAMED_IMPORTS", JavaScriptLanguage.INSTANCE, ES6NamedImportsImpl::new);

    IElementType IMPORT_SPECIFIER =
        new ElementTypeAsPsiFactory("IMPORT_SPECIFIER", JavaScriptLanguage.INSTANCE, ES6ImportSpecifierImpl::new);
}

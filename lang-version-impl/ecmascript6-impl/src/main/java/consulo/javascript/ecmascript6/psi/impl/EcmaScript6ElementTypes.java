package consulo.javascript.ecmascript6.psi.impl;

import com.intellij.psi.tree.IElementType;
import consulo.javascript.lang.JavaScriptLanguage;
import consulo.psi.tree.ElementTypeAsPsiFactory;

/**
 * @author VISTALL
 * @since 2019-12-14
 */
public interface EcmaScript6ElementTypes
{
	IElementType EXPORT_DEFAULT_ASSIGMENT = new ElementTypeAsPsiFactory("EXPORT_DEFAULT_ASSIGMENT", JavaScriptLanguage.INSTANCE, ES6ExportDefaultAssignmentImpl.class);

	IElementType IMPORT_DECLARATION = new ElementTypeAsPsiFactory("IMPORT_DECLARATION", JavaScriptLanguage.INSTANCE, ES6ImportDeclarationImpl.class);

	IElementType IMPORTED_BINDING = new ElementTypeAsPsiFactory("IMPORTED_BINDING", JavaScriptLanguage.INSTANCE, ES6ImportedBindingImpl.class);

	IElementType NAMED_IMPORTS = new ElementTypeAsPsiFactory("NAMED_IMPORTS", JavaScriptLanguage.INSTANCE, ES6NamedImportsImpl.class);

	IElementType IMPORT_SPECIFIER = new ElementTypeAsPsiFactory("IMPORT_SPECIFIER", JavaScriptLanguage.INSTANCE, ES6ImportSpecifierImpl.class);
}

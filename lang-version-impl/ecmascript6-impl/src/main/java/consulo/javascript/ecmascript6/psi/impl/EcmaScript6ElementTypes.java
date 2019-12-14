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
}

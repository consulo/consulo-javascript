package consulo.javascript.lang.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.impl.JSLiteralExpressionImpl;

/**
 * Own implementation for RegExp literal for correct registration RegExpLanguageHost.
 *
 * String literal with regexp will have own RegExp host
 *
 * @author VISTALL
 * @since 21/12/2021
 */
public class JSRegExpLiteralExpressionImpl extends JSLiteralExpressionImpl
{
	public JSRegExpLiteralExpressionImpl(ASTNode node)
	{
		super(node);
	}
}

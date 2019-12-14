package consulo.javascript.ecmascript6.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.impl.JSStatementImpl;
import consulo.javascript.ecmascript6.psi.ES6ImportDeclaration;

/**
 * @author VISTALL
 * @since 2019-12-14
 */
public class ES6ImportDeclarationImpl extends JSStatementImpl implements ES6ImportDeclaration
{
	public ES6ImportDeclarationImpl(ASTNode node)
	{
		super(node);
	}
}

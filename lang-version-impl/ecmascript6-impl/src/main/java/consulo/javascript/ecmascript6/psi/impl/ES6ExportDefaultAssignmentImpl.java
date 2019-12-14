package consulo.javascript.ecmascript6.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.impl.JSStatementImpl;
import consulo.javascript.ecmascript6.psi.ES6ExportDefaultAssignment;

/**
 * @author VISTALL
 * @since 2019-12-14
 */
public class ES6ExportDefaultAssignmentImpl extends JSStatementImpl implements ES6ExportDefaultAssignment
{
	public ES6ExportDefaultAssignmentImpl(ASTNode node)
	{
		super(node);
	}
}

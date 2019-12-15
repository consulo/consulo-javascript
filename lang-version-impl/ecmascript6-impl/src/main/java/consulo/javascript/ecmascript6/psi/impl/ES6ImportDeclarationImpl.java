package consulo.javascript.ecmascript6.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.impl.JSStatementImpl;
import com.intellij.psi.PsiElement;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import consulo.javascript.ecmascript6.psi.ES6ImportDeclaration;

import javax.annotation.Nonnull;

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

	@Override
	public boolean processDeclarations(@Nonnull PsiScopeProcessor processor, @Nonnull ResolveState state, PsiElement lastParent, @Nonnull PsiElement place)
	{
		for(PsiElement child : getChildren())
		{
			if(!child.processDeclarations(processor, state, lastParent, place))
			{
				return false;
			}
		}
		return true;
	}
}

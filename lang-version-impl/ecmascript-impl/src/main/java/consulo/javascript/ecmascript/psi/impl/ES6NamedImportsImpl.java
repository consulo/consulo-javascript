package consulo.javascript.ecmascript.psi.impl;

import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.impl.JSElementImpl;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.ecmascript.psi.ES6ImportedBinding;
import consulo.javascript.ecmascript.psi.ES6NamedImports;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;

import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2019-12-14
 */
public class ES6NamedImportsImpl extends JSElementImpl implements ES6NamedImports
{
	public ES6NamedImportsImpl(ASTNode node)
	{
		super(node);
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSElement(this);
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

	@RequiredReadAction
	@Nonnull
	@Override
	public ES6ImportedBinding[] getBindings()
	{
		return findChildrenByClass(ES6ImportedBinding.class);
	}
}

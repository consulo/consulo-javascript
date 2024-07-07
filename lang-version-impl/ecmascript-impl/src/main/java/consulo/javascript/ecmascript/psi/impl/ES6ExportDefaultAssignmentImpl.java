package consulo.javascript.ecmascript.psi.impl;

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.impl.JSStatementImpl;
import consulo.javascript.ecmascript.psi.ES6ExportDefaultAssignment;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSElement(this);
	}

	@Nullable
	public JSElement getElement()
	{
		return findChildByClass(JSClass.class);
	}

	@Override
	public boolean processDeclarations(@Nonnull PsiScopeProcessor processor, @Nonnull ResolveState state, PsiElement lastParent, @Nonnull PsiElement place)
	{
		JSElement element = getElement();
		if(element != null && !element.processDeclarations(processor, state, lastParent, place))
		{
			return false;
		}
		return super.processDeclarations(processor, state, lastParent, place);
	}
}

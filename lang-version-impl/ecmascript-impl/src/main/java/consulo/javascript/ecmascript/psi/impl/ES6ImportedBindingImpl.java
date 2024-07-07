package consulo.javascript.ecmascript.psi.impl;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.impl.JSElementImpl;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.javascript.ecmascript.psi.ES6ImportSpecifier;
import consulo.javascript.ecmascript.psi.ES6ImportedBinding;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.util.IncorrectOperationException;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * @author VISTALL
 * @since 2019-12-14
 */
public class ES6ImportedBindingImpl extends JSElementImpl implements ES6ImportedBinding
{
	public ES6ImportedBindingImpl(ASTNode node)
	{
		super(node);
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSElement(this);
	}

	@RequiredReadAction
	@Override
	public String getName()
	{
		PsiElement nameIdentifier = getNameIdentifier();
		return nameIdentifier == null ? null : nameIdentifier.getText();
	}

	@RequiredReadAction
	@Nullable
	@Override
	public PsiElement getNameIdentifier()
	{
		ES6ImportSpecifier importSpecifier = getImportSpecifier();
		if(importSpecifier != null)
		{
			return importSpecifier.getNameIdentifier();
		}
		return findChildByType(JSTokenTypes.IDENTIFIER);
	}

	@RequiredReadAction
	@Override
	public int getTextOffset()
	{
		PsiElement nameIdentifier = getNameIdentifier();
		return nameIdentifier == null ? super.getTextOffset() : nameIdentifier.getTextOffset();
	}

	@RequiredWriteAction
	@Override
	public PsiElement setName(@Nonnull String s) throws IncorrectOperationException
	{
		return null;
	}

	@Override
	public boolean processDeclarations(@Nonnull PsiScopeProcessor processor, @Nonnull ResolveState state, PsiElement lastParent, @Nonnull PsiElement place)
	{
		return processor.execute(this, state);
	}

	@RequiredReadAction
	@Nullable
	@Override
	public ES6ImportSpecifier getImportSpecifier()
	{
		return findChildByClass(ES6ImportSpecifier.class);
	}
}

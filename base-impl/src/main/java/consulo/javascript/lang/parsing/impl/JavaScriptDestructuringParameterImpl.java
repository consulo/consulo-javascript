package consulo.javascript.lang.parsing.impl;

import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSElementImpl;
import com.intellij.lang.javascript.psi.stubs.JSVariableStubBase;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.javascript.language.psi.JavaScriptType;
import consulo.javascript.language.psi.JavaScriptTypeElement;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.util.IncorrectOperationException;
import org.jetbrains.annotations.NonNls;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * @author VISTALL
 * @since 2019-12-17
 */
public class JavaScriptDestructuringParameterImpl extends JSElementImpl implements JSDestructuringParameter
{
	public JavaScriptDestructuringParameterImpl(ASTNode node)
	{
		super(node);
	}

	@Override
	@RequiredReadAction
	public boolean processDeclarations(@Nonnull PsiScopeProcessor processor, @Nonnull ResolveState state, PsiElement lastParent, @Nonnull PsiElement place)
	{
		JSDestructuringObject destructuringObject = getDestructuringObject();
		if(destructuringObject == null)
		{
			return true;
		}
		for(JSVariable variable : destructuringObject.getVariables())
		{
			if(!processor.execute(variable, state))
			{
				return false;
			}
		}
		return super.processDeclarations(processor, state, lastParent, place);
	}

	@Nullable
	@Override
	public JSVariableStubBase getStub()
	{
		return null;
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSElement(this);
	}

	@RequiredReadAction
	@Nullable
	@Override
	public JSDestructuringObject getDestructuringObject()
	{
		return findChildByClass(JSDestructuringObject.class);
	}

	@Override
	public JSFunction getDeclaringFunction()
	{
		return getStubOrPsiParentOfType(JSFunction.class);
	}

	@RequiredReadAction
	@Override
	public boolean isRest()
	{
		return false;
	}

	@RequiredReadAction
	@Nullable
	@Override
	public PsiElement getRestElement()
	{
		return null;
	}

	@Override
	public boolean isOptional()
	{
		return false;
	}

	@Override
	public boolean hasInitializer()
	{
		return false;
	}

	@RequiredReadAction
	@Nullable
	@Override
	public JSExpression getInitializer()
	{
		return null;
	}

	@Override
	public String getInitializerText()
	{
		return null;
	}

	@Override
	public void setInitializer(JSExpression expr) throws IncorrectOperationException
	{

	}

	@Nonnull
	@Override
	public JavaScriptType getType()
	{
		return JavaScriptType.UNKNOWN;
	}

	@Nullable
	@Override
	public String getTypeString()
	{
		return null;
	}

	@RequiredReadAction
	@Nullable
	@Override
	public JavaScriptTypeElement getTypeElement()
	{
		return null;
	}

	@Override
	public boolean isConst()
	{
		return false;
	}

	@Override
	public boolean isLocal()
	{
		return false;
	}

	@Override
	public boolean isDeprecated()
	{
		return false;
	}

	@Nullable
	@Override
	public JSAttributeList getAttributeList()
	{
		return null;
	}

	@Override
	public String getQualifiedName()
	{
		return null;
	}

	@RequiredReadAction
	@Nullable
	@Override
	public PsiElement getNameIdentifier()
	{
		return null;
	}

	@RequiredWriteAction
	@Override
	public PsiElement setName(@Nonnull @NonNls String name) throws IncorrectOperationException
	{
		return null;
	}
}

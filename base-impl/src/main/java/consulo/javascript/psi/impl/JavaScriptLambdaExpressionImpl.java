/*
 * Copyright 2013-2016 must-be.org
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package consulo.javascript.psi.impl;

import javax.annotation.Nonnull;

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSParameterList;
import com.intellij.lang.javascript.psi.impl.JSExpressionImpl;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.psi.JavaScriptLambdaExpression;

/**
 * @author VISTALL
 * @since 03.03.2016
 */
public class JavaScriptLambdaExpressionImpl extends JSExpressionImpl implements JavaScriptLambdaExpression
{
	public JavaScriptLambdaExpressionImpl(ASTNode node)
	{
		super(node);
	}

	@Override
	@RequiredReadAction
	public boolean processDeclarations(@Nonnull PsiScopeProcessor processor, @Nonnull ResolveState state, PsiElement lastParent, @Nonnull PsiElement place)
	{
		if(lastParent != null && lastParent.getParent() == this)
		{
			final JSParameter[] params = getParameterList().getParameters();
			for(JSParameter param : params)
			{
				if(!processor.execute(param, state))
				{
					return false;
				}
			}

			boolean b = JSResolveUtil.processDeclarationsInScope(this, processor, state, lastParent, place);
			if(b)
			{
				processor.handleEvent(PsiScopeProcessor.Event.SET_DECLARATION_HOLDER, this);
			}
			return b;
		}

		return processor.execute(this, state);
	}

	@Override
	public void accept(@Nonnull PsiElementVisitor visitor)
	{
		if(visitor instanceof JSElementVisitor)
		{
			((JSElementVisitor) visitor).visitLambdaExpression(this);
		}
		else
		{
			visitor.visitElement(this);
		}
	}

	@RequiredReadAction
	@Nonnull
	@Override
	public JSParameterList getParameterList()
	{
		return findNotNullChildByClass(JSParameterList.class);
	}
}

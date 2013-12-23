/*
 * Copyright 2000-2005 JetBrains s.r.o.
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
package com.intellij.lang.javascript.psi.impl;

import java.util.ArrayList;
import java.util.List;

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSArrayLiteralExpression;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.tree.IElementType;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 11:32:23 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSArrayLiteralExpressionImpl extends JSExpressionImpl implements JSArrayLiteralExpression
{
	public JSArrayLiteralExpressionImpl(final ASTNode node)
	{
		super(node);
	}

	@Override
	public JSExpression[] getExpressions()
	{
		List<JSExpression> result = new ArrayList<JSExpression>();
		ASTNode child = getNode().getFirstChildNode();
		boolean wasExpression = false;
		while(child != null)
		{
			final IElementType type = child.getElementType();
			if(JSElementTypes.EXPRESSIONS.contains(type))
			{
				result.add((JSExpression) child.getPsi());
				wasExpression = true;
			}
			else if(type == JSTokenTypes.COMMA)
			{
				if(wasExpression)
				{
					wasExpression = false;
				}
				else
				{
					result.add(null); // Skipped expression like [a,,b]
				}
			}
			child = child.getTreeNext();
		}

		return result.toArray(new JSExpression[result.size()]);
	}

	@Override
	public void accept(@NotNull PsiElementVisitor visitor)
	{
		if(visitor instanceof JSElementVisitor)
		{
			((JSElementVisitor) visitor).visitJSArrayLiteralExpression(this);
		}
		else
		{
			visitor.visitElement(this);
		}
	}
}

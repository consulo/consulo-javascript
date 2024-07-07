/*
 * Copyright 2005-2006 Olivier Descout
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
package org.intellij.idea.lang.javascript.intention.bool;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSExpression;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSMutablyNamedIntention;
import org.intellij.idea.lang.javascript.psiutil.BoolUtils;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.jetbrains.annotations.NonNls;

import jakarta.annotation.Nonnull;

@ExtensionImpl
@IntentionMetaData(ignoreId = "JSDeMorgansLawIntention", categories = {
		"JavaScript",
		"Boolean"
}, fileExtensions = "js")
public class JSDeMorgansLawIntention extends JSMutablyNamedIntention
{
	@NonNls
	private static final String AND_OPERATOR = "&&";
	@NonNls
	private static final String OR_OPERATOR = "||";
	@NonNls
	private static final String AND_SUFFIX = "ANDAND";
	@NonNls
	private static final String OR_SUFFIX = "OROR";

	@Override
	protected String getTextForElement(PsiElement element)
	{
		final IElementType tokenType = ((JSBinaryExpression) element).getOperationSign();

		return this.getSuffixedDisplayName(tokenType.equals(JSTokenTypes.ANDAND) ? AND_SUFFIX : OR_SUFFIX);
	}

	@Override
	@Nonnull
	public JSElementPredicate getElementPredicate()
	{
		return new ConjunctionPredicate();
	}

	@Override
	public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException
	{
		JSBinaryExpression exp = (JSBinaryExpression) element;
		final IElementType tokenType = exp.getOperationSign();
		JSElement parent = (JSElement) exp.getParent();

		while(isConjunctionExpression(parent, tokenType))
		{
			exp = (JSBinaryExpression) parent;
			assert (exp != null);
			parent = (JSElement) exp.getParent();
		}

		final String newExpression = this.convertConjunctionExpression(exp, tokenType);

		JSElementFactory.replaceExpressionWithNegatedExpressionString(exp, newExpression);
	}

	private String convertConjunctionExpression(JSBinaryExpression exp,
												IElementType tokenType)
	{
		final String leftText = this.getOperandText(exp.getLOperand(), tokenType);
		final String rightText = this.getOperandText(exp.getROperand(), tokenType);
		final String flippedConjunction = (tokenType.equals(JSTokenTypes.ANDAND) ? OR_OPERATOR : AND_OPERATOR);

		return leftText + flippedConjunction + rightText;
	}

	private String getOperandText(JSExpression operand, IElementType tokenType)
	{
		return (isConjunctionExpression(operand, tokenType)
				? this.convertConjunctionExpression((JSBinaryExpression) operand, tokenType)
				: BoolUtils.getNegatedExpressionText(operand));
	}

	private static boolean isConjunctionExpression(JSElement exp,
												   IElementType conjunctionType)
	{
		if(!(exp instanceof JSBinaryExpression))
		{
			return false;
		}

		final JSBinaryExpression binaryExpression = (JSBinaryExpression) exp;
		final IElementType tokenType = binaryExpression.getOperationSign();

		return tokenType.equals(conjunctionType);
	}
}

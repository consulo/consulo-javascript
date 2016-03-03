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

package org.mustbe.consulo.javascript.lang.parsing;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptBundle;
import com.intellij.psi.tree.IElementType;

/**
 * @author VISTALL
 * @since 03.03.2016
 */
public class EcmaScript6ExpressionParsing extends ExpressionParsing
{
	public EcmaScript6ExpressionParsing(JavaScriptParsingContext context)
	{
		super(context);
	}

	@Override
	protected void parseProperty(PsiBuilder builder)
	{
		final IElementType nameToken = builder.getTokenType();
		final PsiBuilder.Marker property = builder.mark();

		IElementType tokenType = builder.getTokenType();
		if(tokenType == JSTokenTypes.LBRACKET)
		{
			PsiBuilder.Marker mark = builder.mark();
			builder.advanceLexer();
			parseExpression(builder);
			checkMatches(builder, JSTokenTypes.RBRACKET, JavaScriptBundle.message("javascript.parser.message.expected.rbracket"));
			mark.done(JSElementTypes.COMPUTED_NAME);
		}
		else
		{
			if(isNotPropertyStart(builder, nameToken))
			{
				builder.error(JavaScriptBundle.message("javascript.parser.message.expected.identifier.string.literal.or.numeric.literal"));
			}
			builder.advanceLexer();
		}

		checkMatches(builder, JSTokenTypes.COLON, JavaScriptBundle.message("javascript.parser.message.expected.colon"));

		builder.putUserData(WITHIN_OBJECT_LITERAL_EXPRESSION, Boolean.TRUE);
		if(!parseAssignmentExpression(builder))
		{
			builder.error(JavaScriptBundle.message("javascript.parser.message.expected.expression"));
		}
		builder.putUserData(WITHIN_OBJECT_LITERAL_EXPRESSION, null);

		property.done(JSElementTypes.PROPERTY);
	}

	@Override
	public boolean isNotPropertyStart(PsiBuilder builder, IElementType elementType)
	{
		return super.isNotPropertyStart(builder, elementType) && elementType != JSTokenTypes.LBRACKET;
	}
}

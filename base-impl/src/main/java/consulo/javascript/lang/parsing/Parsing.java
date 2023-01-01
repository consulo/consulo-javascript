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

package consulo.javascript.lang.parsing;

import com.intellij.lang.javascript.JSTokenTypes;
import consulo.javascript.lang.JavaScriptContextKeywordElementType;
import consulo.javascript.lang.parsing.impl.JavaScriptStrictParserBuilder;
import consulo.language.ast.IElementType;
import consulo.language.ast.TokenSet;
import consulo.language.parser.PsiBuilder;
import consulo.localize.LocalizeValue;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * User: max
 * Date: Jan 28, 2005
 * Time: 7:03:42 PM
 */
public class Parsing<C extends JavaScriptParsingContext>
{
	private C myContext;

	public Parsing(C context)
	{
		myContext = context;
	}

	public boolean isContextKeyword(@Nonnull PsiBuilder builder, @Nonnull IElementType elementType)
	{
		if(builder.getTokenType() == JSTokenTypes.IDENTIFIER)
		{
			IElementType contextKeywordElementType = JavaScriptContextKeywordElementType.getKeywordByText(builder.getTokenText());
			if(contextKeywordElementType == null)
			{
				return false;
			}
			if(elementType == contextKeywordElementType)
			{
				return true;
			}
		}
		return false;
	}

	@Nullable
	public IElementType isContextKeyword(@Nonnull PsiBuilder builder, @Nonnull TokenSet tokenSet)
	{
		if(builder.getTokenType() == JSTokenTypes.IDENTIFIER)
		{
			IElementType contextKeywordElementType = JavaScriptContextKeywordElementType.getKeywordByText(builder.getTokenText());
			if(contextKeywordElementType == null)
			{
				return null;
			}
			if(tokenSet.contains(contextKeywordElementType))
			{
				return contextKeywordElementType;
			}
		}
		return null;
	}

	public void advanceContextKeyword(@Nonnull PsiBuilder builder, @Nonnull IElementType elementType)
	{
		if(isContextKeyword(builder, elementType))
		{
			if(builder instanceof JavaScriptStrictParserBuilder)
			{
				((JavaScriptStrictParserBuilder) builder).disableNonStrictRemap(builder.getCurrentOffset());
			}
			builder.remapCurrentToken(elementType);
			builder.advanceLexer();
		}
	}

	public void advanceContextKeyword(@Nonnull PsiBuilder builder, @Nonnull TokenSet tokenSet)
	{
		IElementType elementType = isContextKeyword(builder, tokenSet);
		if(elementType != null)
		{
			if(builder instanceof JavaScriptStrictParserBuilder)
			{
				((JavaScriptStrictParserBuilder) builder).disableNonStrictRemap(builder.getCurrentOffset());
			}
			builder.remapCurrentToken(elementType);
			builder.advanceLexer();
		}
	}

	public FunctionParsing getFunctionParsing()
	{
		return myContext.getFunctionParsing();
	}

	public StatementParsing getStatementParsing()
	{
		return myContext.getStatementParsing();
	}

	public ExpressionParsing getExpressionParsing()
	{
		return myContext.getExpressionParsing();
	}

	public static void buildTokenElement(IElementType type, PsiBuilder builder)
	{
		final PsiBuilder.Marker marker = builder.mark();
		builder.advanceLexer();
		marker.done(type);
	}

	@Deprecated
	public static boolean checkMatches(final PsiBuilder builder, final IElementType token, final String message)
	{
		if(builder.getTokenType() == token)
		{
			builder.advanceLexer();
			return true;
		}
		else
		{
			builder.error(message);
			return false;
		}
	}

	public static boolean checkMatches(final PsiBuilder builder, final IElementType token, final LocalizeValue message)
	{
		if(builder.getTokenType() == token)
		{
			builder.advanceLexer();
			return true;
		}
		else
		{
			builder.error(message);
			return false;
		}
	}

	public boolean isIdentifierToken(PsiBuilder builder, IElementType tokenType)
	{
		return myContext.isIdentifierToken(builder, tokenType);
	}

	public boolean isIdentifierToken(PsiBuilder builder)
	{
		return myContext.isIdentifierName(builder, builder.getTokenType());
	}

	public boolean isIdentifierName(PsiBuilder builder, IElementType tokenType)
	{
		return myContext.isIdentifierName(builder, tokenType);
	}
}

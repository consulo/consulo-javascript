package org.mustbe.consulo.json.lang;

import org.jetbrains.annotations.NotNull;
import org.mustbe.consulo.javascript.lang.parsing.JavaScriptParser;
import org.mustbe.consulo.javascript.lang.parsing.JavaScriptParsingContext;
import com.intellij.lang.ASTNode;
import com.intellij.lang.LanguageVersion;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.psi.tree.IElementType;

/**
 * @author VISTALL
 * @since 05.03.2015
 */
public class JsonJavaScriptParser extends JavaScriptParser
{
	@NotNull
	@Override
	public ASTNode parse(@NotNull IElementType root, @NotNull PsiBuilder builder, @NotNull LanguageVersion languageVersion)
	{
		final PsiBuilder.Marker rootMarker = builder.mark();
		JavaScriptParsingContext parsingContext = createParsingContext();
		parseRoot(builder, parsingContext.getExpressionParsing());
		rootMarker.done(root);
		return builder.getTreeBuilt();
	}

	private void parseRoot(PsiBuilder builder, org.mustbe.consulo.javascript.lang.parsing.ExpressionParsing expressionParsing)
	{
		if(builder.getTokenType() == JSTokenTypes.LBRACKET)
		{
			expressionParsing.parseArrayLiteralExpression(builder);
			if(builder.getTokenType() != null)
			{
				builder.error(JSBundle.message("javascript.parser.message.expected.eof"));
			}
		}
		else if(builder.getTokenType() == JSTokenTypes.LBRACE)
		{
			expressionParsing.parseObjectLiteralExpression(builder);
			if(builder.getTokenType() != null)
			{
				builder.error(JSBundle.message("javascript.parser.message.expected.eof"));
			}
		}
		else
		{
			builder.error(JSBundle.message("javascript.parser.message.expected.lbrace.or.lbracket"));
		}

		while(builder.getTokenType() != null)
		{
			builder.advanceLexer();
		}
	}
}

package consulo.javascript.lang.parsing;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import consulo.language.ast.ICustomParsingType;
import consulo.language.ast.IElementType;
import consulo.language.ast.ILazyParseableElementType;
import consulo.language.ast.TokenType;
import consulo.language.parser.PsiBuilder;
import consulo.xml.codeInsight.daemon.XmlErrorMessages;
import consulo.xml.psi.xml.XmlElementType;
import consulo.xml.psi.xml.XmlTokenType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import java.util.Stack;

/**
 * @author VISTALL
 * @see XmlParsing
 * @see HtmlParsing
 * @since 2019-12-17
 */
public class JSXParser
{
	private final Stack<String> myTagNamesStack = new Stack<>();
	private static final int BALANCING_DEPTH_THRESHOLD = 1000;

	private PsiBuilder myBuilder;

	public void setBuilder(PsiBuilder builder)
	{
		myBuilder = builder;
	}

	private PsiBuilder.Marker mark()
	{
		return myBuilder.mark();
	}

	private void advance()
	{
		myBuilder.advanceLexer();
	}

	private IElementType token()
	{
		return myBuilder.getTokenType();
	}

	private void error(String error)
	{
		myBuilder.error(error);
	}

	private boolean eof()
	{
		return myBuilder.eof();
	}

	@Nullable
	private String parseTagHeader(final boolean multipleRootTagError, final PsiBuilder.Marker tag)
	{
		if(multipleRootTagError)
		{
			final PsiBuilder.Marker error = mark();
			advance();
			error.error(XmlErrorMessages.message("xml.parsing.multiple.root.tags"));
		}
		else
		{
			advance();
		}

		final String tagName;
		if(token() != JSTokenTypes.XML_NAME || myBuilder.rawLookup(-1) == TokenType.WHITE_SPACE)
		{
			error(XmlErrorMessages.message("xml.parsing.tag.name.expected"));
			tagName = "";
		}
		else
		{
			tagName = myBuilder.getTokenText();
			assert tagName != null;
			advance();
		}
		myTagNamesStack.push(tagName);

		do
		{
			final IElementType tt = token();
			if(tt == JSTokenTypes.XML_NAME)
			{
				parseAttribute();
			}
			else if(tt == JSTokenTypes.XML_JS_SCRIPT)
			{
				advance();
			}
			else
			{
				break;
			}
		}
		while(true);

		if(token() == XmlTokenType.XML_EMPTY_ELEMENT_END)
		{
			advance();
			myTagNamesStack.pop();
			tag.done(JSElementTypes.XML_LITERAL_EXPRESSION);
			return null;
		}

		if(token() == JSTokenTypes.XML_TAG_END)
		{
			advance();
		}
		else
		{
			error(XmlErrorMessages.message("tag.start.is.not.closed"));
			myTagNamesStack.pop();
			tag.done(JSElementTypes.XML_LITERAL_EXPRESSION);
			return null;
		}

		if(myTagNamesStack.size() > BALANCING_DEPTH_THRESHOLD)
		{
			error(XmlErrorMessages.message("way.too.unbalanced"));
			tag.done(JSElementTypes.XML_LITERAL_EXPRESSION);
			return null;
		}

		return tagName;
	}


	private void parseAttribute()
	{
		assert token() == XmlTokenType.XML_NAME;
		final PsiBuilder.Marker att = mark();
		advance();
		if(token() == XmlTokenType.XML_EQ)
		{
			advance();
			parseAttributeValue();
		}
		att.done(XmlElementType.XML_ATTRIBUTE);
	}

	private void parseAttributeValue()
	{
		final PsiBuilder.Marker attValue = mark();
		if(token() == XmlTokenType.XML_ATTRIBUTE_VALUE_START_DELIMITER)
		{
			while(true)
			{
				final IElementType tt = token();
				if(tt == null || tt == XmlTokenType.XML_ATTRIBUTE_VALUE_END_DELIMITER || tt == XmlTokenType.XML_END_TAG_START || tt == XmlTokenType.XML_EMPTY_ELEMENT_END ||
						tt == JSTokenTypes.XML_START_TAG_START)
				{
					break;
				}

				if(tt == JSTokenTypes.BAD_CHARACTER)
				{
					final PsiBuilder.Marker error = mark();
					advance();
					error.error(XmlErrorMessages.message("unescaped.ampersand.or.nonterminated.character.entity.reference"));
				}
				else
				{
					advance();
				}
			}

			if(token() == XmlTokenType.XML_ATTRIBUTE_VALUE_END_DELIMITER)
			{
				advance();
			}
			else
			{
				error(XmlErrorMessages.message("xml.parsing.unclosed.attribute.value"));
			}
		}
		else if(token() == JSTokenTypes.XML_JS_SCRIPT)
		{
			advance();
		}
		else
		{
			if(token() != XmlTokenType.XML_TAG_END && token() != XmlTokenType.XML_EMPTY_ELEMENT_END)
			{
				advance(); // Single token att value
			}
		}

		attValue.done(XmlElementType.XML_ATTRIBUTE_VALUE);
	}

	public void parseTagContent()
	{
		PsiBuilder.Marker xmlText = null;
		while(true)
		{
			final IElementType tt = token();
			if(tt == null || tt == XmlTokenType.XML_END_TAG_START)
			{
				break;
			}

			if(tt == JSTokenTypes.XML_START_TAG_START)
			{
				xmlText = terminateText(xmlText);
				parseTag(false);
			}
			else if(tt == JSTokenTypes.BAD_CHARACTER)
			{
				xmlText = startText(xmlText);
				final PsiBuilder.Marker error = mark();
				advance();
				error.error(XmlErrorMessages.message("unescaped.ampersand.or.nonterminated.character.entity.reference"));
			}
			else if(tt instanceof ICustomParsingType || tt instanceof ILazyParseableElementType)
			{
				xmlText = terminateText(xmlText);
				advance();
			}
			else
			{
				xmlText = startText(xmlText);
				advance();
			}
		}

		terminateText(xmlText);
	}

	@Nullable
	private static PsiBuilder.Marker terminateText(@Nullable PsiBuilder.Marker xmlText)
	{
		if(xmlText != null)
		{
			xmlText.done(XmlElementType.XML_TEXT);
		}
		return null;
	}

	@Nonnull
	private PsiBuilder.Marker startText(@Nullable PsiBuilder.Marker xmlText)
	{
		if(xmlText == null)
		{
			xmlText = mark();
		}
		return xmlText;
	}

	protected void parseTag(boolean multipleRootTagError)
	{
		assert token() == JSTokenTypes.XML_START_TAG_START : "Tag start expected";
		final PsiBuilder.Marker tag = mark();

		final String tagName = parseTagHeader(multipleRootTagError, tag);
		if(tagName == null)
		{
			return;
		}

		final PsiBuilder.Marker content = mark();
		parseTagContent();

		if(token() == JSTokenTypes.XML_END_TAG_START)
		{
			final PsiBuilder.Marker footer = mark();
			advance();

			if(token() == JSTokenTypes.XML_NAME)
			{
				String endName = myBuilder.getTokenText();
				if(!tagName.equals(endName) && myTagNamesStack.contains(endName))
				{
					footer.rollbackTo();
					myTagNamesStack.pop();
					tag.doneBefore(JSElementTypes.XML_LITERAL_EXPRESSION, content, XmlErrorMessages.message("named.element.is.not.closed", tagName));
					content.drop();
					return;
				}

				advance();
			}
			footer.drop();

			while(token() != XmlTokenType.XML_TAG_END && token() != XmlTokenType.XML_START_TAG_START && token() != XmlTokenType.XML_END_TAG_START && !eof())
			{
				error(XmlErrorMessages.message("xml.parsing.unexpected.token"));
				advance();
			}

			if(token() == JSTokenTypes.XML_TAG_END)
			{
				advance();
			}
			else
			{
				error(XmlErrorMessages.message("xml.parsing.closing.tag.is.not.done"));
			}
		}
		else
		{
			error(XmlErrorMessages.message("xml.parsing.unexpected.end.of.file"));
		}

		content.drop();
		myTagNamesStack.pop();
		tag.done(JSElementTypes.XML_LITERAL_EXPRESSION);
	}
}

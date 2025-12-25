package consulo.javascript.lang.parsing;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import consulo.language.ast.ICustomParsingType;
import consulo.language.ast.IElementType;
import consulo.language.ast.ILazyParseableElementType;
import consulo.language.ast.TokenType;
import consulo.language.parser.PsiBuilder;
import consulo.localize.LocalizeValue;
import consulo.xml.impl.localize.XmlErrorLocalize;
import consulo.xml.psi.xml.XmlElementType;
import consulo.xml.psi.xml.XmlTokenType;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.Stack;

/**
 * @author VISTALL
 * @see consulo.xml.psi.impl.source.parsing.xml.XmlParsing
 * @see consulo.xml.lang.html.HtmlParsing
 * @since 2019-12-17
 */
public class JSXParser {
    public static final String FRAGMENT_TAG = "<<<FRAGMENT TAG>>>";

    private final Stack<String> myTagNamesStack = new Stack<>();
    private static final int BALANCING_DEPTH_THRESHOLD = 1000;

    private PsiBuilder myBuilder;

    public void setBuilder(PsiBuilder builder) {
        myBuilder = builder;
    }

    private PsiBuilder.Marker mark() {
        return myBuilder.mark();
    }

    private void advance() {
        myBuilder.advanceLexer();
    }

    private IElementType token() {
        return myBuilder.getTokenType();
    }

    private void error(LocalizeValue error) {
        myBuilder.error(error);
    }

    private boolean eof() {
        return myBuilder.eof();
    }

    @Nullable
    private String parseTagHeader(boolean multipleRootTagError, PsiBuilder.Marker tag) {
        if (token() == JSTokenTypes.XML_START_TAG_LIST) {
            myTagNamesStack.push(FRAGMENT_TAG);
            advance();
            return FRAGMENT_TAG;
        }

        if (multipleRootTagError) {
            PsiBuilder.Marker error = mark();
            advance();
            error.error(XmlErrorLocalize.xmlParsingMultipleRootTags());
        }
        else {
            advance();
        }

        String tagName;
        if (token() != JSTokenTypes.XML_NAME || myBuilder.rawLookup(-1) == TokenType.WHITE_SPACE) {
            error(XmlErrorLocalize.xmlParsingTagNameExpected());
            tagName = "";
        }
        else {
            tagName = myBuilder.getTokenText();
            advance();
        }
        myTagNamesStack.push(tagName);

        do {
            IElementType tt = token();
            if (tt == JSTokenTypes.XML_NAME) {
                parseAttribute();
            }
            else if (tt == JSTokenTypes.XML_JS_SCRIPT) {
                advance();
            }
            else {
                break;
            }
        }
        while (true);

        if (token() == XmlTokenType.XML_EMPTY_ELEMENT_END) {
            advance();
            myTagNamesStack.pop();
            tag.done(JSElementTypes.XML_LITERAL_EXPRESSION);
            return null;
        }

        if (token() == JSTokenTypes.XML_TAG_END) {
            advance();
        }
        else {
            error(XmlErrorLocalize.tagStartIsNotClosed());
            myTagNamesStack.pop();
            tag.done(JSElementTypes.XML_LITERAL_EXPRESSION);
            return null;
        }

        if (myTagNamesStack.size() > BALANCING_DEPTH_THRESHOLD) {
            error(XmlErrorLocalize.wayTooUnbalanced());
            tag.done(JSElementTypes.XML_LITERAL_EXPRESSION);
            return null;
        }

        return tagName;
    }

    private void parseAttribute() {
        PsiBuilder.Marker att = mark();
        advance();
        if (token() == XmlTokenType.XML_EQ) {
            advance();
            parseAttributeValue();
        }
        att.done(XmlElementType.XML_ATTRIBUTE);
    }

    private void parseAttributeValue() {
        PsiBuilder.Marker attValue = mark();
        if (token() == XmlTokenType.XML_ATTRIBUTE_VALUE_START_DELIMITER) {
            while (true) {
                IElementType tt = token();
                if (tt == null || tt == XmlTokenType.XML_ATTRIBUTE_VALUE_END_DELIMITER || tt == XmlTokenType.XML_END_TAG_START
                    || tt == XmlTokenType.XML_EMPTY_ELEMENT_END || tt == JSTokenTypes.XML_START_TAG_START) {
                    break;
                }

                if (tt == JSTokenTypes.BAD_CHARACTER) {
                    PsiBuilder.Marker error = mark();
                    advance();
                    error.error(XmlErrorLocalize.unescapedAmpersandOrNonterminatedCharacterEntityReference());
                }
                else {
                    advance();
                }
            }

            if (token() == XmlTokenType.XML_ATTRIBUTE_VALUE_END_DELIMITER) {
                advance();
            }
            else {
                error(XmlErrorLocalize.xmlParsingUnclosedAttributeValue());
            }
        }
        else if (token() == JSTokenTypes.XML_JS_SCRIPT) {
            advance();
        }
        else if (token() != XmlTokenType.XML_TAG_END && token() != XmlTokenType.XML_EMPTY_ELEMENT_END) {
            advance(); // Single token att value
        }

        attValue.done(XmlElementType.XML_ATTRIBUTE_VALUE);
    }

    public void parseTagContent() {
        PsiBuilder.Marker xmlText = null;
        while (true) {
            IElementType tt = token();
            if (tt == null || tt == XmlTokenType.XML_END_TAG_START) {
                break;
            }

            if (tt == JSTokenTypes.XML_START_TAG_START) {
                xmlText = terminateText(xmlText);
                parseTag(false);
            }
            else if (tt == JSTokenTypes.BAD_CHARACTER) {
                xmlText = startText(xmlText);
                PsiBuilder.Marker error = mark();
                advance();
                error.error(XmlErrorLocalize.unescapedAmpersandOrNonterminatedCharacterEntityReference());
            }
            else if (tt instanceof ICustomParsingType || tt instanceof ILazyParseableElementType) {
                xmlText = terminateText(xmlText);
                advance();
            }
            else {
                xmlText = startText(xmlText);
                advance();
            }
        }

        terminateText(xmlText);
    }

    @Nullable
    private static PsiBuilder.Marker terminateText(@Nullable PsiBuilder.Marker xmlText) {
        if (xmlText != null) {
            xmlText.done(XmlElementType.XML_TEXT);
        }
        return null;
    }

    @Nonnull
    private PsiBuilder.Marker startText(@Nullable PsiBuilder.Marker xmlText) {
        if (xmlText == null) {
            xmlText = mark();
        }
        return xmlText;
    }

    protected void parseTag(boolean multipleRootTagError) {
        PsiBuilder.Marker tag = mark();

        String tagName = parseTagHeader(multipleRootTagError, tag);
        if (tagName == null) {
            return;
        }

        PsiBuilder.Marker content = mark();
        parseTagContent();

        if (token() == JSTokenTypes.XML_END_TAG_START) {
            PsiBuilder.Marker footer = mark();
            advance();

            if (token() == JSTokenTypes.XML_NAME) {
                String endName = myBuilder.getTokenText();
                if (!tagName.equals(endName) && myTagNamesStack.contains(endName)) {
                    footer.rollbackTo();
                    myTagNamesStack.pop();
                    tag.doneBefore(JSElementTypes.XML_LITERAL_EXPRESSION, content, XmlErrorLocalize.namedElementIsNotClosed(tagName));
                    content.drop();
                    return;
                }

                advance();
            }
            footer.drop();

            while (token() != XmlTokenType.XML_TAG_END && token() != XmlTokenType.XML_START_TAG_START
                && token() != XmlTokenType.XML_END_TAG_START && !eof()) {
                error(XmlErrorLocalize.xmlParsingUnexpectedToken());
                advance();
            }

            if (token() == JSTokenTypes.XML_TAG_END) {
                advance();
            }
            else {
                error(XmlErrorLocalize.xmlParsingClosingTagIsNotDone());
            }
        }
        else {
            error(XmlErrorLocalize.xmlParsingUnexpectedEndOfFile());
        }

        content.drop();
        myTagNamesStack.pop();
        tag.done(JSElementTypes.XML_LITERAL_EXPRESSION);
    }
}

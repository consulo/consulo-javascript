package consulo.javascript.lang.lexer;

import consulo.language.lexer.FlexLexer;

/**
 * @author VISTALL
 * @since 2019-12-05
 */
public interface JavaScriptFlexLexer extends FlexLexer {
    void setTagCount(int count);

    int getTagCount();
}

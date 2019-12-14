package consulo.javascript.lang.lexer;

import com.intellij.lexer.FlexLexer;

/**
 * @author VISTALL
 * @since 2019-12-05
 */
public interface JavaScriptFlexLexer extends FlexLexer
{
	void setTagCount(int count);

	int getTagCount();
}

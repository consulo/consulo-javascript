package org.mustbe.consulo.javascript.lang;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.psi.tree.TokenSet;

/**
 * @author VISTALL
 * @since 24.08.14
 */
public interface JSTokenSets extends JSTokenTypes
{
	TokenSet STRING_LITERALS = TokenSet.create(STRING_LITERAL, SINGLE_QUOTE_STRING_LITERAL);

	TokenSet WHITE_SPACES = TokenSet.create(WHITE_SPACE);
}

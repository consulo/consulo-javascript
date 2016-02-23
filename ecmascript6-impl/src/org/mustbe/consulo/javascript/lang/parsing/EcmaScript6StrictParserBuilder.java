package org.mustbe.consulo.javascript.lang.parsing;

import org.mustbe.consulo.javascript.lang.parsing.impl.JavaScriptStrictParserBuilder;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.javascript.JSTokenTypes;

/**
 * @author VISTALL
 * @since 15.02.2016
 */
public class EcmaScript6StrictParserBuilder extends JavaScriptStrictParserBuilder
{
	public EcmaScript6StrictParserBuilder(PsiBuilder delegate)
	{
		super(delegate);

		onlyInStrictMode(JSTokenTypes.IMPLEMENTS_KEYWORD);
		onlyInStrictMode(JSTokenTypes.PACKAGE_KEYWORD);
		onlyInStrictMode(JSTokenTypes.PUBLIC_KEYWORD);
		onlyInStrictMode(JSTokenTypes.PRIVATE_KEYWORD);
		onlyInStrictMode(JSTokenTypes.PROTECTED_KEYWORD);
		onlyInStrictMode(JSTokenTypes.INTERFACE_KEYWORD);
		onlyInStrictMode(JSTokenTypes.STATIC_KEYWORD);
	}
}

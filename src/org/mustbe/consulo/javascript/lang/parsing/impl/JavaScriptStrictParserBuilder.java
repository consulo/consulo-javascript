package org.mustbe.consulo.javascript.lang.parsing.impl;

import java.util.HashMap;
import java.util.Map;

import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.javascript.lang.JavaScriptConstants;
import org.mustbe.consulo.javascript.lang.JavaScriptTokenSets;
import org.mustbe.consulo.javascript.lang.parsing.JavaScriptParserBuilder;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.ThreeState;

/**
 * @author VISTALL
 * @since 15.02.2016
 */
public class JavaScriptStrictParserBuilder extends JavaScriptParserBuilder
{
	private static final int ourExpectedStringSize = JavaScriptConstants.USE_STRICT.length() + 2; // '' or ""

	private ThreeState myStrictState = ThreeState.UNSURE;

	private Map<IElementType, ThreeState> myStrictKeywords = new HashMap<IElementType, ThreeState>();

	public JavaScriptStrictParserBuilder(PsiBuilder delegate)
	{
		super(delegate);
	}

	public void onlyInStrictMode(IElementType elementType)
	{
		myStrictKeywords.put(elementType, ThreeState.YES);
	}

	@Nullable
	@Override
	public IElementType getTokenType()
	{
		checkStrictMode();
		IElementType tokenType = super.getTokenType();
		ThreeState threeState = myStrictKeywords.get(tokenType);
		if(threeState != null)
		{
			if(threeState == myStrictState)
			{
				return tokenType;
			}
			else
			{
				return JSTokenTypes.IDENTIFIER;
			}
		}
		return tokenType;
	}

	@Override
	public void advanceLexer()
	{
		IElementType tokenType = getTokenType();

		IElementType originalTokenType = super.getTokenType();

		if(tokenType != originalTokenType)
		{
			remapCurrentToken(tokenType);
		}

		super.advanceLexer();
	}

	public boolean isStrictMode()
	{
		return myStrictState == ThreeState.YES;
	}

	private void checkStrictMode()
	{
		if(myStrictState != ThreeState.UNSURE)
		{
			return;
		}

		IElementType tokenType = super.getTokenType();
		if(JavaScriptTokenSets.STRING_LITERALS.contains(tokenType))
		{
			String tokenText = getTokenText();
			if(tokenText == null)
			{
				return;
			}

			int length = tokenText.length();
			if(ourExpectedStringSize == length)
			{
				String stringContent = StringUtil.unquoteString(tokenText);
				if(JavaScriptConstants.USE_STRICT.equals(stringContent))
				{
					myStrictState = ThreeState.YES;
					return;
				}
			}
		}
		myStrictState = ThreeState.NO;
	}
}

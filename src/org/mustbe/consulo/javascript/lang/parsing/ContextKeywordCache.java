package org.mustbe.consulo.javascript.lang.parsing;

import java.util.HashMap;
import java.util.Map;

import org.jetbrains.annotations.Nullable;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.psi.tree.IElementType;

/**
 * @author VISTALL
 * @since 15.02.2016
 */
public class ContextKeywordCache
{
	private static Map<String, IElementType> ourCache = new HashMap<String, IElementType>();

	static
	{
		ourCache.put("get", JSTokenTypes.GET_KEYWORD);
		ourCache.put("set", JSTokenTypes.SET_KEYWORD);
	}

	@Nullable
	public static IElementType getContextKeywordElementType(String text)
	{
		return ourCache.get(text);
	}
}

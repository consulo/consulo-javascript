package org.mustbe.consulo.json.jom.proxy.impl;

import java.lang.reflect.Type;

import org.jetbrains.annotations.NotNull;
import org.mustbe.consulo.RequiredReadAction;
import org.mustbe.consulo.javascript.lang.JavaScriptTokenSets;
import org.mustbe.consulo.json.jom.proxy.JomBadValueExpressionException;
import org.mustbe.consulo.json.jom.proxy.JomValueConverter;
import com.intellij.lang.javascript.psi.JSSimpleLiteralExpression;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;

/**
 * @author VISTALL
 * @since 13.11.2015
 */
public class JomStringConverter implements JomValueConverter.Converter<String>
{
	@Override
	public String getDefaultValue()
	{
		return null;
	}

	@RequiredReadAction
	@Override
	public String parseValue(@NotNull Class type, @NotNull Type genericType, @NotNull PsiElement value) throws JomBadValueExpressionException
	{
		if(value instanceof JSSimpleLiteralExpression)
		{
			IElementType elementType = ((JSSimpleLiteralExpression) value).getLiteralElementType();
			if(JavaScriptTokenSets.STRING_LITERALS.contains(elementType))
			{
				return StringUtil.unquoteString(value.getText());
			}
		}

		throw new JomBadValueExpressionException();
	}
}

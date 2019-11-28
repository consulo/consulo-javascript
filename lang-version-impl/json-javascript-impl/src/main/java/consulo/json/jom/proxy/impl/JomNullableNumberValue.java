package consulo.json.jom.proxy.impl;

import java.lang.reflect.Method;
import java.lang.reflect.Type;

import javax.annotation.Nonnull;

import consulo.annotation.access.RequiredReadAction;
import consulo.json.jom.proxy.JomBadValueExpressionException;
import consulo.json.jom.proxy.JomValueConverter;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import com.intellij.openapi.util.NotNullLazyValue;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiUtilCore;
import com.intellij.util.ReflectionUtil;

/**
 * @author VISTALL
 * @since 13.11.2015
 */
public class JomNullableNumberValue<T> implements JomValueConverter.Converter<T>
{
	protected Class<T> myClass;

	protected NotNullLazyValue<Method> myParseMethodValue = new NotNullLazyValue<Method>()
	{
		@Nonnull
		@Override
		@SuppressWarnings("unchecked")
		protected Method compute()
		{
			Class primitiveType = ReflectionUtil.getStaticFieldValue(myClass, Class.class, "TYPE");

			Method method = ReflectionUtil.getMethod(myClass, "parse" + StringUtil.capitalize(primitiveType.getSimpleName()), String.class);
			assert method != null;
			return method;
		}
	};

	public JomNullableNumberValue(Class<T> aClass)
	{
		myClass = aClass;
	}

	@Override
	public T getDefaultValue()
	{
		return null;
	}

	@RequiredReadAction
	@Override
	@SuppressWarnings("unchecked")
	public T parseValue(@Nonnull Class type, @Nonnull Type genericType, @Nonnull PsiElement value) throws JomBadValueExpressionException
	{
		if(value instanceof JSLiteralExpression)
		{
			IElementType elementType = PsiUtilCore.getElementType(value.getFirstChild());
			if(elementType == JSTokenTypes.NUMERIC_LITERAL)
			{
				try
				{
					return (T) myParseMethodValue.getValue().invoke(null, value.getText());
				}
				catch(Exception e)
				{
					throw new JomBadValueExpressionException(e);
				}
			}
		}

		throw new JomBadValueExpressionException();
	}
}

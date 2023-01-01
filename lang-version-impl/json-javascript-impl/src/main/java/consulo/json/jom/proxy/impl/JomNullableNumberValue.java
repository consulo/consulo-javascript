package consulo.json.jom.proxy.impl;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.json.jom.proxy.JomBadValueExpressionException;
import consulo.json.jom.proxy.JomValueConverter;
import consulo.language.ast.IElementType;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiUtilCore;
import consulo.util.lang.StringUtil;
import consulo.util.lang.lazy.LazyValue;
import consulo.util.lang.reflect.ReflectionUtil;

import javax.annotation.Nonnull;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.function.Supplier;

/**
 * @author VISTALL
 * @since 13.11.2015
 */
public class JomNullableNumberValue<T> implements JomValueConverter.Converter<T>
{
	protected Class<T> myClass;

	protected Supplier<Method> myParseMethodValue;

	public JomNullableNumberValue(Class<T> aClass)
	{
		myClass = aClass;
		myParseMethodValue = LazyValue.notNull(() ->
		{
			Class primitiveType = ReflectionUtil.getStaticFieldValue(myClass, Class.class, "TYPE");

			Method method = ReflectionUtil.getMethod(myClass, "parse" + StringUtil.capitalize(primitiveType.getSimpleName()), String.class);
			assert method != null;
			return method;
		});
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
					return (T) myParseMethodValue.get().invoke(null, value.getText());
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

package consulo.json.jom.proxy.impl;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Collection;

import jakarta.annotation.Nonnull;

import consulo.annotation.access.RequiredReadAction;
import consulo.json.jom.proxy.JomBadValueExpressionException;
import consulo.json.jom.proxy.JomValueConverter;
import com.intellij.lang.javascript.psi.JSArrayLiteralExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import consulo.language.psi.PsiElement;
import consulo.util.lang.Pair;
import consulo.util.lang.reflect.ReflectionUtil;

/**
 * @author VISTALL
 * @since 13.11.2015
 */
public class JomCollectionValue<T extends Collection> implements JomValueConverter.Converter<T>
{
	private Class<? extends T> myNewInstanceClass;

	public JomCollectionValue(Class<? extends T> newInstanceClass)
	{
		myNewInstanceClass = newInstanceClass;
	}

	@Override
	public T getDefaultValue()
	{
		return ReflectionUtil.newInstance(myNewInstanceClass);
	}

	@RequiredReadAction
	@Override
	@SuppressWarnings("unchecked")
	public T parseValue(@Nonnull Class type, @Nonnull Type genericType, @Nonnull PsiElement value) throws JomBadValueExpressionException
	{
		if(value instanceof JSArrayLiteralExpression)
		{
			JSExpression[] expressions = ((JSArrayLiteralExpression) value).getExpressions();
			T collection = ReflectionUtil.newInstance(myNewInstanceClass);

			Pair<Class, Type> valueType = findValueTypeInsideGeneric(genericType, 0); // E

			for(JSExpression expression : expressions)
			{
				try
				{
					Object o = JomValueConverter.convertToObject(valueType.getFirst(), valueType.getSecond(), expression);
					collection.add(o);
				}
				catch(JomBadValueExpressionException e)
				{
					// we dont interest in bad value
				}
			}
			return collection;
		}
		throw new JomBadValueExpressionException();
	}


	public static Pair<Class, Type> findValueTypeInsideGeneric(@Nonnull Type genericType, int index)
	{
		if(!(genericType instanceof ParameterizedType))
		{
			throw new JomBadValueExpressionException();
		}

		Type[] actualTypeArguments = ((ParameterizedType) genericType).getActualTypeArguments();
		if(actualTypeArguments.length <= index)
		{
			throw new JomBadValueExpressionException();
		}

		Class rawType = null;
		Type valueType = actualTypeArguments[index];
		if(valueType instanceof ParameterizedType)
		{
			rawType = (Class) ((ParameterizedType) valueType).getRawType();
		}
		else
		{
			rawType = (Class) valueType;
		}
		return Pair.create(rawType, valueType);
	}
}

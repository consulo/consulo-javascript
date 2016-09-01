package org.mustbe.consulo.json.jom.proxy;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import consulo.annotations.RequiredReadAction;
import org.mustbe.consulo.json.jom.JomElement;
import org.mustbe.consulo.json.jom.JomUtil;
import com.intellij.lang.javascript.psi.JSObjectLiteralExpression;
import com.intellij.lang.javascript.psi.JSProperty;
import com.intellij.openapi.util.Comparing;

/**
 * @author VISTALL
 * @since 13.11.2015
 */
public class JomProxyInvocationHandler implements InvocationHandler
{
	@NotNull
	public static JomElement createProxy(@NotNull Class<?> interfaceClass, @Nullable final JSObjectLiteralExpression objectLiteralExpression)
	{
		return (JomElement) Proxy.newProxyInstance(interfaceClass.getClassLoader(), new Class[]{interfaceClass}, new JomProxyInvocationHandler(objectLiteralExpression));
	}

	private JSObjectLiteralExpression myObjectLiteralExpression;

	public JomProxyInvocationHandler(@Nullable JSObjectLiteralExpression objectLiteralExpression)
	{
		myObjectLiteralExpression = objectLiteralExpression;
	}

	@Override
	@RequiredReadAction
	public Object invoke(Object proxy, Method method, Object[] args) throws Throwable
	{
		String propertyName = JomUtil.getJsonGetPropertyName(method);
		if(propertyName != null)
		{
			JSProperty jsProperty = findProperty(myObjectLiteralExpression, propertyName);
			if(jsProperty == null)
			{
				return JomValueConverter.getDefaultValueForType(method.getReturnType());
			}
			else
			{
				try
				{
					return JomValueConverter.convertToObject(method.getReturnType(), method.getGenericReturnType(), jsProperty.getValue());
				}
				catch(JomBadValueExpressionException e)
				{
					return JomValueConverter.getDefaultValueForType(method.getReturnType());
				}
			}
		}
		return null;
	}


	@Nullable
	private static JSProperty findProperty(@Nullable JSObjectLiteralExpression objectLiteralExpression, String name)
	{
		if(objectLiteralExpression == null)
		{
			return null;
		}
		for(JSProperty property : objectLiteralExpression.getProperties())
		{
			if(Comparing.equal(property.getName(), name))
			{
				return property;
			}
		}
		return null;
	}
}

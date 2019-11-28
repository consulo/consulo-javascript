package consulo.json.jom.proxy.impl;

import java.lang.reflect.Type;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.annotation.Nonnull;
import consulo.annotation.access.RequiredReadAction;
import consulo.json.jom.proxy.JomBadValueExpressionException;
import consulo.json.jom.proxy.JomValueConverter;
import com.intellij.lang.javascript.psi.JSObjectLiteralExpression;
import com.intellij.lang.javascript.psi.JSProperty;
import com.intellij.openapi.util.Pair;
import com.intellij.psi.PsiElement;

/**
 * @author VISTALL
 * @since 13.11.2015
 */
public class JomMapConverter implements JomValueConverter.Converter<Map>
{
	@Override
	public Map getDefaultValue()
	{
		return Collections.emptyMap();
	}

	@RequiredReadAction
	@Override
	@SuppressWarnings("unchecked")
	public Map parseValue(@Nonnull Class type, @Nonnull Type genericType, @Nonnull PsiElement value) throws JomBadValueExpressionException
	{
		if(!(value instanceof JSObjectLiteralExpression))
		{
			throw new JomBadValueExpressionException();
		}

		Pair<Class, Type> valueType = JomCollectionValue.findValueTypeInsideGeneric(genericType, 1); // K, V

		JSProperty[] properties = ((JSObjectLiteralExpression) value).getProperties();

		Map map = new LinkedHashMap(properties.length);

		for(JSProperty property : properties)
		{
			String name = property.getName();
			if(name == null)
			{
				continue;
			}

			try
			{
				Object object = JomValueConverter.convertToObject(valueType.getFirst(), valueType.getSecond(), property.getValue());
				map.put(name, object);
			}
			catch(JomBadValueExpressionException e)
			{
				// we dont interest in bad value
			}
		}

		return map;
	}

}

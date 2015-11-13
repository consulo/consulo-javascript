package org.mustbe.consulo.json.jom;

import java.lang.reflect.Method;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.ObjectUtil;

/**
 * @author VISTALL
 * @since 13.11.2015
 */
public class JomUtil
{
	@Nullable
	public static String getJsonGetPropertyName(@NotNull Method method)
	{
		JomPropertyGetter annotation = method.getAnnotation(JomPropertyGetter.class);
		if(annotation == null)
		{
			return null;
		}

		String propertyName = StringUtil.getPropertyName(method.getName());
		propertyName = ObjectUtil.notNull(propertyName, method.getName());
		if(!StringUtil.isEmpty(annotation.value()))
		{
			propertyName = annotation.value();
		}
		return propertyName;
	}
}

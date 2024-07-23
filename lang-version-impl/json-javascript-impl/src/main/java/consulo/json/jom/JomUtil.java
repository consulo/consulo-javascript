package consulo.json.jom;

import consulo.util.lang.ObjectUtil;
import consulo.util.lang.StringUtil;
import jakarta.annotation.Nonnull;

import jakarta.annotation.Nullable;

import java.lang.reflect.Method;

/**
 * @author VISTALL
 * @since 13.11.2015
 */
public class JomUtil {
    @Nullable
    public static String getJsonGetPropertyName(@Nonnull Method method) {
        JomPropertyGetter annotation = method.getAnnotation(JomPropertyGetter.class);
        if (annotation == null) {
            return null;
        }

        String propertyName = StringUtil.getPropertyName(method.getName());
        propertyName = ObjectUtil.notNull(propertyName, method.getName());
        if (!StringUtil.isEmpty(annotation.value())) {
            propertyName = annotation.value();
        }
        return propertyName;
    }
}

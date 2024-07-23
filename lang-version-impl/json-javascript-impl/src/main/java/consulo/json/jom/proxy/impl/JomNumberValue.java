package consulo.json.jom.proxy.impl;

import consulo.json.jom.proxy.JomBadValueExpressionException;

/**
 * @author VISTALL
 * @since 13.11.2015
 */
public class JomNumberValue<T> extends JomNullableNumberValue<T> {
    public JomNumberValue(Class<T> aClass) {
        super(aClass);
    }

    @Override
    @SuppressWarnings("unchecked")
    public T getDefaultValue() {
        try {
            return (T)myParseMethodValue.get().invoke(null, "0");
        }
        catch (Exception e) {
            throw new JomBadValueExpressionException(e);
        }
    }
}

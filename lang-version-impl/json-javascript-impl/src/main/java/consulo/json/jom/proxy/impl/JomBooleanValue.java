package consulo.json.jom.proxy.impl;

import java.lang.reflect.Type;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import consulo.annotation.access.RequiredReadAction;
import consulo.json.jom.proxy.JomBadValueExpressionException;
import consulo.json.jom.proxy.JomValueConverter;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import consulo.language.ast.IElementType;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiUtilCore;

/**
 * @author VISTALL
 * @since 13.11.2015
 */
public class JomBooleanValue implements JomValueConverter.Converter<Boolean> {
    private Boolean myDefaultValue;

    public JomBooleanValue(@Nullable Boolean defaultValue) {
        myDefaultValue = defaultValue;
    }

    @Override
    public Boolean getDefaultValue() {
        return myDefaultValue;
    }

    @RequiredReadAction
    @Override
    public Boolean parseValue(
        @Nonnull Class type,
        @Nonnull Type genericType,
        @Nonnull PsiElement value
    ) throws JomBadValueExpressionException {
        if (value instanceof JSLiteralExpression) {
            IElementType elementType = PsiUtilCore.getElementType(value.getFirstChild());
            if (elementType == JSTokenTypes.TRUE_KEYWORD) {
                return Boolean.TRUE;
            }
            else if (elementType == JSTokenTypes.FALSE_KEYWORD) {
                return Boolean.FALSE;
            }
        }
        throw new JomBadValueExpressionException();
    }
}

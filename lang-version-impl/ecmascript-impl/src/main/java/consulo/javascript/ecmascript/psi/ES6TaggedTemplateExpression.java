package consulo.javascript.ecmascript.psi;

import com.intellij.lang.javascript.psi.JSExpression;
import consulo.annotation.access.RequiredReadAction;
import org.jspecify.annotations.Nullable;

/**
 * @author VISTALL
 * @since 2026-10-18
 *
 * tag`text ${expression} text`
 */
public interface ES6TaggedTemplateExpression extends JSExpression {
    @RequiredReadAction
    @Nullable
    JSExpression getTag();

    @RequiredReadAction
    @Nullable
    ES6StringTemplateExpression getTemplateExpression();
}

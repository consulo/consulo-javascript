package consulo.javascript.ecmascript.psi;

import com.intellij.lang.javascript.psi.JSExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.document.util.TextRange;
import consulo.language.psi.PsiLanguageInjectionHost;

/**
 * @author VISTALL
 * @since 2026-10-18
 *
 * `text ${expression} text`
 */
public interface ES6StringTemplateExpression extends JSExpression, PsiLanguageInjectionHost {
    @RequiredReadAction
    JSExpression[] getArguments();

    @RequiredReadAction
    TextRange[] getStringRanges();

    @RequiredReadAction
    TextRange[] getStringRangesWithEmpty();
}

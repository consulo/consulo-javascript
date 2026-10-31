package consulo.javascript.ecmascript.inject;

import com.intellij.lang.javascript.patterns.JSPatterns;
import com.intellij.lang.javascript.psi.JSExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.document.util.TextRange;
import consulo.javascript.ecmascript.psi.ES6StringTemplateExpression;
import consulo.language.inject.advanced.AbstractLanguageInjectionSupport;
import consulo.language.inject.advanced.BaseInjection;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiLanguageInjectionHost;
import org.jdom.Element;

import java.util.List;

/**
 * Language injections into the strings of JavaScript and TypeScript, configured by places of {@link JSPatterns}
 * ({@code jsString().and(jsAssignedTo("innerHTML"))}): the defaults of {@code jsInjections.xml} and the ones of the
 * user (Settings | Language Injections).
 *
 * @since 2026-10-31
 */
@ExtensionImpl
public class JavaScriptLanguageInjectionSupport extends AbstractLanguageInjectionSupport {
    public static final String SUPPORT_ID = "js";

    @Override
    public String getId() {
        return SUPPORT_ID;
    }

    @Override
    public Class[] getPatternClasses() {
        return new Class[]{JSPatterns.class};
    }

    @Override
    public boolean isApplicableTo(PsiLanguageInjectionHost host) {
        return host instanceof JSExpression;
    }

    @Override
    public boolean useDefaultInjector(PsiLanguageInjectionHost host) {
        return true;
    }

    @Override
    public BaseInjection createInjection(Element element) {
        return new BaseInjection(SUPPORT_ID) {
            /**
             * The text of a string without the quotes; of a template literal without the substitutions.
             */
            @Override
            @RequiredReadAction
            public List<TextRange> getInjectedArea(PsiElement element) {
                if (element instanceof ES6StringTemplateExpression template) {
                    return List.of(template.getStringRanges());
                }
                int length = element.getTextLength();
                return length >= 2 ? List.of(new TextRange(1, length - 1)) : List.of();
            }
        };
    }
}

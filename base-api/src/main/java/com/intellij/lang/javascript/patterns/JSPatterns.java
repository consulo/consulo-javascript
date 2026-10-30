package com.intellij.lang.javascript.patterns;

import com.intellij.lang.javascript.psi.JSArgumentList;
import com.intellij.lang.javascript.psi.JSCallExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import com.intellij.lang.javascript.psi.JSProperty;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import consulo.language.pattern.ElementPattern;
import consulo.language.pattern.PatternCondition;
import consulo.language.pattern.PlatformPatterns;
import consulo.language.psi.PsiElement;
import consulo.language.util.ProcessingContext;

/**
 * Factory of JavaScript (and TypeScript) PSI patterns: IntelliJ's API.
 *
 * @since 2026-10-31
 */
public class JSPatterns extends PlatformPatterns {
    public static JSElementPattern.Capture<JSExpression> jsExpression() {
        return new JSElementPattern.Capture<>(JSExpression.class);
    }

    public static JSElementPattern.Capture<JSLiteralExpression> jsLiteralExpression() {
        return new JSElementPattern.Capture<>(JSLiteralExpression.class);
    }

    /**
     * A literal that also matches the pattern.
     */
    public static JSElementPattern.Capture<JSLiteralExpression> jsLiteralExpression(ElementPattern<? extends JSExpression> pattern) {
        return jsLiteralExpression().and(pattern);
    }

    public static JSElementPattern.Capture<JSReferenceExpression> jsReferenceExpression() {
        return new JSElementPattern.Capture<>(JSReferenceExpression.class);
    }

    public static JSElementPattern.Capture<JSProperty> jsProperty() {
        return new JSElementPattern.Capture<>(JSProperty.class);
    }

    /**
     * The argument {@code index} of a call ({@code f(a)}, {@code new F(a)}) whose callee matches the pattern.
     */
    public static JSElementPattern.Capture<JSExpression> jsArgument(ElementPattern<? extends JSReferenceExpression> callee, int index) {
        return jsExpression().with(new PatternCondition<JSExpression>("jsArgument") {
            @Override
            public boolean accepts(JSExpression expression, ProcessingContext context) {
                PsiElement parent = expression.getParent();
                if (!(parent instanceof JSArgumentList argumentList)) {
                    return false;
                }
                JSExpression[] arguments = argumentList.getArguments();
                if (index < 0 || index >= arguments.length || arguments[index] != expression) {
                    return false;
                }
                return argumentList.getParent() instanceof JSCallExpression call && callee.accepts(call.getMethodExpression(), context);
            }
        });
    }

    /**
     * {@link #jsArgument(ElementPattern, int)} for a callee with this text ({@code "Routing.generate"}).
     */
    public static JSElementPattern.Capture<JSExpression> jsArgument(String calleeQualifiedName, int index) {
        return jsArgument(jsReferenceExpression().withQualifiedName(calleeQualifiedName), index);
    }
}

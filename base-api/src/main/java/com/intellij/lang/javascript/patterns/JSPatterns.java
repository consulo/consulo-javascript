package com.intellij.lang.javascript.patterns;

import com.intellij.lang.javascript.psi.JSArgumentList;
import com.intellij.lang.javascript.psi.JSAssignmentExpression;
import com.intellij.lang.javascript.psi.JSCallExpression;
import com.intellij.lang.javascript.psi.JSDefinitionExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import com.intellij.lang.javascript.psi.JSProperty;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import consulo.language.pattern.ElementPattern;
import consulo.language.pattern.PatternCondition;
import consulo.language.pattern.PlatformPatterns;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiLanguageInjectionHost;
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
    /**
     * A string: a quoted literal or a template literal ({@code `<li>${name}</li>`}) - the hosts of injected languages.
     */
    public static JSElementPattern.Capture<JSExpression> jsString() {
        return jsExpression().with(new PatternCondition<JSExpression>("jsString") {
            @Override
            public boolean accepts(JSExpression expression, ProcessingContext context) {
                if (!(expression instanceof PsiLanguageInjectionHost) || expression.getTextLength() < 2) {
                    return false;
                }
                char first = expression.getText().charAt(0);
                return first == '\'' || first == '"' || first == '`';
            }
        });
    }

    /**
     * The value assigned to a target matching the pattern ({@code target = value}).
     */
    public static JSElementPattern.Capture<JSExpression> jsAssignedTo(ElementPattern<? extends JSExpression> target) {
        return jsExpression().with(new PatternCondition<JSExpression>("jsAssignedTo") {
            @Override
            public boolean accepts(JSExpression expression, ProcessingContext context) {
                if (!(expression.getParent() instanceof JSAssignmentExpression assignment) || assignment.getROperand() != expression) {
                    return false;
                }
                JSExpression left = assignment.getLOperand();
                if (left instanceof JSDefinitionExpression definition) {
                    left = definition.getExpression();
                }
                return left != null && target.accepts(left, context);
            }
        });
    }

    /**
     * The value assigned to a property of one of these names ({@code el.innerHTML = value}).
     */
    public static JSElementPattern.Capture<JSExpression> jsAssignedTo(String... propertyNames) {
        return jsAssignedTo(jsReferenceExpression().withReferencedName(propertyNames));
    }

    /**
     * The argument {@code index} of a call of a method of this name, whatever its qualifier
     * ({@code el.insertAdjacentHTML(position, html)}).
     */
    public static JSElementPattern.Capture<JSExpression> jsArgumentOfMethod(String methodName, int index) {
        return jsArgument(jsReferenceExpression().withReferencedName(methodName), index);
    }

    /**
     * An argument of a call whose argument {@code index} is a string literal with this value
     * ({@code parseFromString(html, 'text/html')}).
     */
    public static JSElementPattern.Capture<JSExpression> jsArgumentWith(int index, String value) {
        return jsExpression().with(new PatternCondition<JSExpression>("jsArgumentWith") {
            @Override
            public boolean accepts(JSExpression expression, ProcessingContext context) {
                if (!(expression.getParent() instanceof JSArgumentList argumentList)) {
                    return false;
                }
                JSExpression[] arguments = argumentList.getArguments();
                if (index < 0 || index >= arguments.length || !(arguments[index] instanceof JSLiteralExpression literal)) {
                    return false;
                }
                String text = literal.getText();
                return text.length() == value.length() + 2 && text.regionMatches(1, value, 0, value.length());
            }
        });
    }
}

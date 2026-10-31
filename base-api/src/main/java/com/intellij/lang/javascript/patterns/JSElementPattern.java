package com.intellij.lang.javascript.patterns;

import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSQualifiedNamedElement;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import consulo.language.pattern.InitialPatternCondition;
import consulo.language.pattern.PatternCondition;
import consulo.language.pattern.PsiElementPattern;
import consulo.language.psi.PsiNamedElement;
import consulo.language.util.ProcessingContext;
import org.jspecify.annotations.Nullable;

/**
 * Patterns of JavaScript (and TypeScript) PSI: IntelliJ's API.
 *
 * @since 2026-10-31
 */
public class JSElementPattern<T extends JSElement, Self extends JSElementPattern<T, Self>> extends PsiElementPattern<T, Self> {
    protected JSElementPattern(Class<T> aClass) {
        super(aClass);
    }

    protected JSElementPattern(InitialPatternCondition<T> condition) {
        super(condition);
    }

    /**
     * A reference expression with this text ({@code Routing.generate}, whitespace and {@code ?.} ignored), or a
     * declaration with this qualified name.
     */
    public Self withQualifiedName(String qualifiedName) {
        return with(new PatternCondition<T>("withQualifiedName") {
            @Override
            public boolean accepts(T element, ProcessingContext context) {
                return qualifiedName.equals(getQualifiedName(element));
            }
        });
    }

    /**
     * A named element with this name ({@code jsProperty().withName("url")}).
     */
    public Self withName(String name) {
        return with(new PatternCondition<T>("withName") {
            @Override
            public boolean accepts(T element, ProcessingContext context) {
                return element instanceof PsiNamedElement named && name.equals(named.getName());
            }
        });
    }

    /**
     * A reference expression whose last name is one of these ({@code el.innerHTML} - {@code innerHTML}).
     */
    public Self withReferencedName(String... names) {
        return with(new PatternCondition<T>("withReferencedName") {
            @Override
            public boolean accepts(T element, ProcessingContext context) {
                if (!(element instanceof JSReferenceExpression reference)) {
                    return false;
                }
                String referencedName = reference.getReferencedName();
                for (String name : names) {
                    if (name.equals(referencedName)) {
                        return true;
                    }
                }
                return false;
            }
        });
    }

    @Nullable
    private static String getQualifiedName(JSElement element) {
        if (element instanceof JSReferenceExpression reference) {
            String text = reference.getText();
            return text == null ? null : text.replaceAll("\\s+", "").replace("?.", ".");
        }
        if (element instanceof JSQualifiedNamedElement named) {
            return named.getQualifiedName();
        }
        return null;
    }

    public static class Capture<T extends JSElement> extends JSElementPattern<T, Capture<T>> {
        public Capture(Class<T> aClass) {
            super(aClass);
        }

        public Capture(InitialPatternCondition<T> condition) {
            super(condition);
        }
    }
}

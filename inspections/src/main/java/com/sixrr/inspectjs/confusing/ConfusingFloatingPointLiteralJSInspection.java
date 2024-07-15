package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.psi.JSSimpleLiteralExpression;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;
import jakarta.annotation.Nonnull;
import org.jetbrains.annotations.NonNls;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

@ExtensionImpl
public class ConfusingFloatingPointLiteralJSInspection extends JavaScriptInspection {
    @NonNls
    private static final Pattern PICKY_FLOATING_POINT_PATTERN = Pattern.compile("[0-9]+\\.[0-9]+((e|E)(-)?[0-9]+)?(f|F|d|D)?");

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME.get();
    }

    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.confusingFloatingPointLiteralDisplayName().get();
    }

    @RequiredReadAction
    @Override
    @Nonnull
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.confusingFloatingPointLiteralProblemDescriptor().get();
    }

    @Override
    public InspectionJSFix buildFix(PsiElement location, Object state) {
        return new ConfusingFloatingPointLiteralFix();
    }

    private static class ConfusingFloatingPointLiteralFix extends InspectionJSFix {
        @Override
        @Nonnull
        public String getName() {
            return InspectionJSLocalize.confusingFloatingPointLiteralChangeQuickfix().get();
        }

        @Override
        public void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException {
            final JSExpression literalExpression = (JSExpression) descriptor.getPsiElement();
            final String text = literalExpression.getText();
            final String newText = getCanonicalForm(text);
            replaceExpression(literalExpression, newText);
        }

        private static String getCanonicalForm(String text) {
            final String suffix;
            final String prefix;
            if (text.indexOf((int) 'e') > 0) {
                final int breakPoint = text.indexOf((int) 'e');
                suffix = text.substring(breakPoint);
                prefix = text.substring(0, breakPoint);
            } else if (text.indexOf((int) 'E') > 0) {
                final int breakPoint = text.indexOf((int) 'E');
                suffix = text.substring(breakPoint);
                prefix = text.substring(0, breakPoint);
            } else if (text.indexOf((int) 'f') > 0) {
                final int breakPoint = text.indexOf((int) 'f');
                suffix = text.substring(breakPoint);
                prefix = text.substring(0, breakPoint);
            } else if (text.indexOf((int) 'F') > 0) {
                final int breakPoint = text.indexOf((int) 'F');
                suffix = text.substring(breakPoint);
                prefix = text.substring(0, breakPoint);
            } else if (text.indexOf((int) 'd') > 0) {
                final int breakPoint = text.indexOf((int) 'd');
                suffix = text.substring(breakPoint);
                prefix = text.substring(0, breakPoint);
            } else if (text.indexOf((int) 'D') > 0) {
                final int breakPoint = text.indexOf((int) 'D');
                suffix = text.substring(breakPoint);
                prefix = text.substring(0, breakPoint);
            } else {
                suffix = "";
                prefix = text;
            }
            final int indexPoint = prefix.indexOf((int) '.');
            if (indexPoint < 0) {
                return prefix + ".0" + suffix;
            } else if (indexPoint == 0) {
                return '0' + prefix + suffix;
            } else {
                return prefix + '0' + suffix;
            }

        }
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new ConfusingFloatingPointLiteralVisitor();
    }

    private static class ConfusingFloatingPointLiteralVisitor extends BaseInspectionVisitor {
        @Override
        public void visitJSLiteralExpression(@Nonnull JSSimpleLiteralExpression literal) {
            super.visitJSLiteralExpression(literal);
            final String text = literal.getText();
            if (text == null) {
                return;
            }
            if (!isFloatingPoint(literal)) {
                return;
            }
            if (!isConfusing(text)) {
                return;
            }
            registerError(literal);
        }
    }

    private static boolean isConfusing(String text) {
        final Matcher matcher = PICKY_FLOATING_POINT_PATTERN.matcher(text);
        return !matcher.matches();
    }

    private static boolean isFloatingPoint(JSLiteralExpression literal) {
        final String text = literal.getText();
        final char firstChar = text.charAt(0);
        if (firstChar != '.' && !Character.isDigit(firstChar)) {
            return false;
        }
        return text.contains(".") || text.contains("e") || text.contains("E");
    }
}

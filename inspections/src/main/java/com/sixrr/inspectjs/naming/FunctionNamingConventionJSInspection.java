package com.sixrr.inspectjs.naming;

import com.intellij.lang.javascript.psi.JSFunction;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.fix.RenameFix;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.PsiElement;
import consulo.language.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;

@ExtensionImpl
public class FunctionNamingConventionJSInspection extends ConventionInspection {
    private static final int DEFAULT_MIN_LENGTH = 4;
    private static final int DEFAULT_MAX_LENGTH = 32;
    private final RenameFix fix = new RenameFix();

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("function.naming.convention.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.NAMING_CONVENTIONS_GROUP_NAME;
    }

    @Override
	protected InspectionJSFix buildFix(PsiElement location) {
        return fix;
    }

    @Override
	protected boolean buildQuickFixesOnlyForOnTheFlyErrors() {
        return true;
    }

    @Override
	public String buildErrorString(Object... args) {
        final String functionName = ((PsiElement) args[0]).getText();

        assert functionName != null;
        if (functionName.length() < getMinLength()) {
            return InspectionJSBundle.message("function.name.is.too.short.error.string", functionName);
        } else if (functionName.length() > getMaxLength()) {
            return InspectionJSBundle.message("function.name.is.too.long.error.string", functionName);
        }
        return InspectionJSBundle.message("function.name.doesnt.match.regex.error.string", functionName, getRegex());
    }

    @Override
	@NonNls
    protected String getDefaultRegex() {
        return "[a-z][A-Za-z]*";
    }

    @Override
	protected int getDefaultMinLength() {
        return DEFAULT_MIN_LENGTH;
    }

    @Override
	protected int getDefaultMaxLength() {
        return DEFAULT_MAX_LENGTH;
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private class Visitor extends BaseInspectionVisitor {
        @Override public void visitJSFunctionDeclaration(JSFunction function) {
            super.visitJSFunctionDeclaration(function);

            final String name = function.getName();
            if (name == null) {
                return;
            }
            if (isValid(name)) {
                return;
            }
            final PsiElement identifier = function.getNameIdentifier();
            if (identifier == null ||
                    !PsiTreeUtil.isAncestor(function, identifier, true)) {
                return;
            }
            registerFunctionError(function);
        }
    }
}

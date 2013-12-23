package com.sixrr.inspectjs;

import java.lang.reflect.Method;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.codeInspection.CustomSuppressableInspectionTool;
import com.intellij.codeInspection.LocalInspectionTool;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.codeInspection.SuppressIntentionAction;
import com.intellij.codeInspection.SuppressionUtil;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSSuppressionHolder;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.util.PsiTreeUtil;

public abstract class BaseInspection extends LocalInspectionTool implements CustomSuppressableInspectionTool {
    private final String m_shortName = null;

    @Override
	@NotNull
    public String getShortName() {
        if (m_shortName == null) {
            final Class<? extends BaseInspection> aClass = getClass();
            @NonNls final String name = aClass.getName();
            assert name.endsWith("Inspection") : "class name must end with the 'Inspection' to correctly calculate the short name: "+name;
            return name.substring(name.lastIndexOf((int) '.') + 1,
                    name.length() - "Inspection".length());
        }
        return m_shortName;
    }

  @Override
  @NotNull
    public PsiElementVisitor buildVisitor(@NotNull ProblemsHolder problemsHolder, boolean onTheFly){
        final BaseInspectionVisitor visitor = buildVisitor();
        visitor.setProblemsHolder(problemsHolder);
        visitor.setOnTheFly(onTheFly);
        visitor.setInspection(this);
        return visitor;
    }


    @Nullable
    protected String buildErrorString(Object... args) {
        return null;
    }

    protected boolean buildQuickFixesOnlyForOnTheFlyErrors() {
        return false;
    }

    @Nullable
    protected InspectionJSFix buildFix(PsiElement location) {
        return null;
    }

    @Nullable
    protected InspectionJSFix[] buildFixes(PsiElement location) {
        return null;
    }

    public boolean hasQuickFix() {
        final Method[] methods = getClass().getDeclaredMethods();
        for (final Method method : methods) {
            @NonNls final String methodName = method.getName();
            if ("buildFix".equals(methodName)) {
                return true;
            }
        }
        return false;
    }

    public abstract BaseInspectionVisitor buildVisitor();

    protected boolean functionHasIdentifier(JSFunction function) {
        final ASTNode identifier = function.findNameIdentifier();
        return identifier != null && PsiTreeUtil.isAncestor(function, identifier.getPsi(), true);
    }

    @Override
    public PsiNamedElement getProblemElement(final PsiElement psiElement) {
        return PsiTreeUtil.getNonStrictParentOfType(psiElement, PsiFile.class);
    }

    @Override
	public SuppressIntentionAction[] getSuppressActions(@Nullable final PsiElement element) {
        return new SuppressIntentionAction[] {
           /* new AddNoInspectionCommentFix(HighlightDisplayKey.find(getShortName()), JSSuppressionHolder.class),  */
        };
    }

    @Override
	public boolean isSuppressedFor(final PsiElement element) {
        return SuppressionUtil.isSuppressedInStatement(element, getID(), JSSuppressionHolder.class);
    }
}
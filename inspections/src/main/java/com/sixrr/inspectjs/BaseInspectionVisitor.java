package com.sixrr.inspectjs;

import java.util.List;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import consulo.language.editor.inspection.LocalQuickFix;
import consulo.language.editor.inspection.ProblemDescriptor;
import com.intellij.lang.javascript.psi.JSCallExpression;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSVariable;
import consulo.language.psi.PsiElement;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.editor.inspection.ProblemHighlightType;
import consulo.language.editor.inspection.ProblemsHolder;
import consulo.language.psi.PsiWhiteSpace;

public abstract class BaseInspectionVisitor extends JSElementVisitor{
    private BaseInspection inspection = null;
    private ProblemsHolder problemsHolder = null;
    private boolean onTheFly = false;
    private List<ProblemDescriptor> errors = null;

    public void setInspection(BaseInspection inspection){
        this.inspection = inspection;
    }

    public void setProblemsHolder(ProblemsHolder problemsHolder){
        this.problemsHolder = problemsHolder;
    }

    public void setOnTheFly(boolean onTheFly){
        this.onTheFly = onTheFly;
    }

    protected void registerFunctionCallError(JSCallExpression expression){
        final JSExpression methodExpression = expression.getMethodExpression();
        PsiElement errorLocation = null;

        if (methodExpression instanceof JSReferenceExpression) {
          errorLocation = ((JSReferenceExpression)methodExpression).getReferenceNameElement();
        } else if (methodExpression instanceof JSFunction) {
          final PsiElement node = ((JSFunction)methodExpression).getNameIdentifier();
          if (node != null) errorLocation = node;
          else errorLocation = methodExpression;
        }

        registerError(errorLocation);
    }

    protected void registerStatementError(JSStatement statement, Object... args){
        final PsiElement statementToken = statement.getFirstChild();
        registerError(statementToken, args);
    }

    protected void registerFunctionError(JSFunction function){
        final PsiElement identifier = function.getNameIdentifier();
        if(identifier == null ||
                !PsiTreeUtil.isAncestor(function, identifier, true)){
            registerError(function.getFirstChild());
        } else{
            registerError(identifier);
        }
    }

    protected void registerVariableError(JSVariable variable){
        final PsiElement nameIdentifier = variable.getFirstChild();
        registerError(nameIdentifier);
    }

  protected void registerError(PsiElement location){
        if(location == null){
            return;
        }
        final LocalQuickFix[] fix = createFixes(location);
        final String description = inspection.buildErrorString(location);
        registerError(location, description, fix);
    }

    private void registerError(PsiElement location, String description,
                               LocalQuickFix[] fixes){
        problemsHolder.registerProblem(getEditorErrorLocation(location),
                description,
                getProblemHighlightType(location), fixes);
    }

    protected ProblemHighlightType getProblemHighlightType(PsiElement location) {
        return ProblemHighlightType.GENERIC_ERROR_OR_WARNING;
    }

    protected PsiElement getEditorErrorLocation(final @Nonnull PsiElement location) {
        return location;
    }

    protected void registerError(PsiElement location, Object... args){
        final LocalQuickFix[] fix = createFixes(location);
        final String description = inspection.buildErrorString(args);
        registerError(location, description, fix);
    }

    @Nullable
    private LocalQuickFix[] createFixes(PsiElement location){
        if(!onTheFly &&
                inspection.buildQuickFixesOnlyForOnTheFlyErrors()){
            return null;
        }
        final InspectionJSFix[] fixes = inspection.buildFixes(location);
        if(fixes != null){
            return fixes;
        }
        final InspectionJSFix fix = inspection.buildFix(location);
        if(fix == null){
            return null;
        }
        return new InspectionJSFix[]{fix};
    }

    @Nullable
    public ProblemDescriptor[] getErrors(){
        if(errors == null){
            return null;
        } else{
            final int numErrors = errors.size();
            return errors.toArray(new ProblemDescriptor[numErrors]);
        }
    }

    @Override public void visitWhiteSpace(PsiWhiteSpace space){
        // none of our inspections need to do anything with white space,
        // so this is a performance optimization
    }
}

package com.sixrr.inspectjs;

import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.*;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

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
          final ASTNode node = ((JSFunction)methodExpression).findNameIdentifier();
          if (node != null) errorLocation = node.getPsi();
          else errorLocation = methodExpression;
        }

        registerError(errorLocation);
    }

    protected void registerStatementError(JSStatement statement, Object... args){
        final PsiElement statementToken = statement.getFirstChild();
        registerError(statementToken, args);
    }

    protected void registerFunctionError(JSFunction function){
        final ASTNode identifier = function.findNameIdentifier();
        if(identifier == null ||
                !PsiTreeUtil.isAncestor(function, identifier.getPsi(), true)){
            registerError(function.getFirstChild());
        } else{
            registerError(identifier.getPsi());
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

    protected PsiElement getEditorErrorLocation(final @NotNull PsiElement location) {
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

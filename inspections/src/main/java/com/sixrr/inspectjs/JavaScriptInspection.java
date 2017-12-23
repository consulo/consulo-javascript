package com.sixrr.inspectjs;

public abstract class JavaScriptInspection extends BaseInspection {
   /*
    public ProblemDescriptor[] checkFile(PsiFile file, InspectionManager manager, boolean isOnTheFly) {
      if (!file.isPhysical()) {
            return super.checkFile(file, manager, isOnTheFly);
        }
        if(file instanceof PsiJavaFile)
        {
            return super.checkFile(file, manager, isOnTheFly);
        }
        final BaseInspectionVisitor visitor = (BaseInspectionVisitor) buildVisitor(manager, isOnTheFly);
        file.accept(visitor);
        return visitor.getErrors();
    }
                             */
}


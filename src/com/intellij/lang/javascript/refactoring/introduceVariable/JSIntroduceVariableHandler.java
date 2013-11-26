package com.intellij.lang.javascript.refactoring.introduceVariable;

import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.refactoring.JSBaseIntroduceHandler;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.openapi.project.Project;

/**
 * @author ven
 */
public class JSIntroduceVariableHandler extends JSBaseIntroduceHandler<JSStatement, Settings, JSIntroduceVariableDialog> {

  protected String getRefactoringName() {
    return JSBundle.message("javascript.introduce.variable.title");
  }

  protected String getCannotIntroduceMessagePropertyKey() {
    return "javascript.introduce.variable.error.no.expression.selected";
  }

  protected String getDeclText(Settings settings) {
    return settings.getIntroducedVarType().toString().toLowerCase() + " " + settings.getVariableName();
  }

  protected JSIntroduceVariableDialog createDialog(final Project project, final JSExpression expression, final JSExpression[] occurrences) {
    return new JSIntroduceVariableDialog(project, occurrences, expression);
  }
}

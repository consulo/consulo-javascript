package com.intellij.lang.javascript.refactoring.introduceVariable;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.refactoring.JSBaseIntroduceDialog;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;

import javax.swing.*;

/**
 * @author ven
 */
public class JSIntroduceVariableDialog extends JSBaseIntroduceDialog implements Settings {
  private JCheckBox myReplaceAllCheckBox;
  private JTextField myNameField;
  private JPanel myPanel;

  private JRadioButton myIntroduceLocalVariable;
  private JRadioButton myIntroduceConstant;
  private JRadioButton myIntroduceVariable;
  private JComboBox myVarType;
  private JLabel myVariableTypeLabel;

  private static IntroducedVarType ourLastIntroduceType = IntroducedVarType.VAR;
  private IntroducedVarType myIntroducedVarType;

  protected JSIntroduceVariableDialog(final Project project,
                                      final JSExpression[] occurences,
                                      final JSExpression mainOccurence) {
    super(project, occurences, mainOccurence, "javascript.introduce.variable.title");

    if (ourLastIntroduceType == Settings.IntroducedVarType.CONST) {
      myIntroduceConstant.setSelected(true);
    }
    else if (ourLastIntroduceType == Settings.IntroducedVarType.LET) {
      myIntroduceLocalVariable.setSelected(true);
    } else if (ourLastIntroduceType == Settings.IntroducedVarType.VAR) {
      myIntroduceVariable.setSelected(true);
    }

    doInit();
  }

  protected JTextField getNameField() {
    return myNameField;
  }

  protected JPanel getPanel() {
    return myPanel;
  }

  protected JCheckBox getReplaceAllCheckBox() {
    return myReplaceAllCheckBox;
  }

  protected void doOKAction() {
    super.doOKAction();

    if (!isShowing()) {
      myIntroducedVarType = myIntroduceConstant.isSelected() ?  Settings.IntroducedVarType.CONST:
        myIntroduceLocalVariable.isSelected() ? Settings.IntroducedVarType.LET :
            myIntroduceVariable.isSelected() ? Settings.IntroducedVarType.VAR:Settings.IntroducedVarType.VAR;

      ourLastIntroduceType = myIntroducedVarType;
    }
  }

  public JComboBox getVarTypeField() {
    return myVarType;
  }

  public IntroducedVarType getIntroducedVarType() {
    return myIntroducedVarType;
  }

  @Override
  protected String suggestCandidateName(final JSExpression mainOccurence) {
    return StringUtil.decapitalize(super.suggestCandidateName(mainOccurence));
  }
}

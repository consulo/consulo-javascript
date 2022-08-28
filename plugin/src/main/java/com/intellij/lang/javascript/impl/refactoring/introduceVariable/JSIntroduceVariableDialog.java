/*
 * Copyright 2000-2005 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.intellij.lang.javascript.impl.refactoring.introduceVariable;

import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.impl.refactoring.JSBaseIntroduceDialog;
import consulo.project.Project;
import consulo.util.lang.StringUtil;

/**
 * @author ven
 */
public class JSIntroduceVariableDialog extends JSBaseIntroduceDialog implements Settings
{
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

	protected JSIntroduceVariableDialog(final Project project, final JSExpression[] occurences, final JSExpression mainOccurence)
	{
		super(project, occurences, mainOccurence, "javascript.introduce.variable.title");

		if(ourLastIntroduceType == Settings.IntroducedVarType.CONST)
		{
			myIntroduceConstant.setSelected(true);
		}
		else if(ourLastIntroduceType == Settings.IntroducedVarType.LET)
		{
			myIntroduceLocalVariable.setSelected(true);
		}
		else if(ourLastIntroduceType == Settings.IntroducedVarType.VAR)
		{
			myIntroduceVariable.setSelected(true);
		}

		doInit();
	}

	@Override
	protected JTextField getNameField()
	{
		return myNameField;
	}

	@Override
	protected JPanel getPanel()
	{
		return myPanel;
	}

	@Override
	protected JCheckBox getReplaceAllCheckBox()
	{
		return myReplaceAllCheckBox;
	}

	@Override
	protected void doOKAction()
	{
		super.doOKAction();

		if(!isShowing())
		{
			myIntroducedVarType = myIntroduceConstant.isSelected() ? Settings.IntroducedVarType.CONST : myIntroduceLocalVariable.isSelected() ? Settings
					.IntroducedVarType.LET : myIntroduceVariable.isSelected() ? Settings.IntroducedVarType.VAR : Settings.IntroducedVarType.VAR;

			ourLastIntroduceType = myIntroducedVarType;
		}
	}

	@Override
	public JComboBox getVarTypeField()
	{
		return myVarType;
	}

	@Override
	public IntroducedVarType getIntroducedVarType()
	{
		return myIntroducedVarType;
	}

	@Override
	protected String suggestCandidateName(final JSExpression mainOccurence)
	{
		return StringUtil.decapitalize(super.suggestCandidateName(mainOccurence));
	}
}

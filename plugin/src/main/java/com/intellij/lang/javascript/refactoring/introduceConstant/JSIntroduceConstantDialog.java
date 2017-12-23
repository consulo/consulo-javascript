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

package com.intellij.lang.javascript.refactoring.introduceConstant;

import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.refactoring.JSBaseClassBasedIntroduceDialog;
import com.intellij.openapi.project.Project;

/**
 * @author Maxim.Mossienko
 *         Date: Jul 24, 2008
 *         Time: 8:48:34 PM
 */
class JSIntroduceConstantDialog extends JSBaseClassBasedIntroduceDialog implements JSIntroduceConstantSettings
{
	private JTextField myNameField;
	private JCheckBox myReplaceAllCheckBox;
	private JPanel myPanel;
	private JRadioButton myPrivate;
	private JRadioButton myProtected;
	private JRadioButton myPackageLocal;
	private JRadioButton myPublic;
	private JComboBox myVarType;

	protected JSIntroduceConstantDialog(final Project project, final JSExpression[] occurences, final JSExpression mainOccurence)
	{
		super(project, occurences, mainOccurence, "javascript.introduce.constant.title");

		doInit();
	}

	@Override
	protected JRadioButton getPrivateRadioButton()
	{
		return myPrivate;
	}

	@Override
	protected JRadioButton getPublicRadioButton()
	{
		return myPublic;
	}

	@Override
	protected JRadioButton getProtectedRadioButton()
	{
		return myProtected;
	}

	@Override
	protected JRadioButton getPackageLocalRadioButton()
	{
		return myPackageLocal;
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
	public JComboBox getVarTypeField()
	{
		return myVarType;
	}

	@Override
	protected String suggestCandidateName(final JSExpression mainOccurence)
	{
		return super.suggestCandidateName(mainOccurence).toUpperCase();
	}
}

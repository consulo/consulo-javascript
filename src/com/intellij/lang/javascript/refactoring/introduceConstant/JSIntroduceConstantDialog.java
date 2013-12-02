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

	protected JRadioButton getPrivateRadioButton()
	{
		return myPrivate;
	}

	protected JRadioButton getPublicRadioButton()
	{
		return myPublic;
	}

	protected JRadioButton getProtectedRadioButton()
	{
		return myProtected;
	}

	protected JRadioButton getPackageLocalRadioButton()
	{
		return myPackageLocal;
	}

	protected JTextField getNameField()
	{
		return myNameField;
	}

	protected JPanel getPanel()
	{
		return myPanel;
	}

	protected JCheckBox getReplaceAllCheckBox()
	{
		return myReplaceAllCheckBox;
	}

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

package com.intellij.lang.javascript.refactoring;

import javax.swing.ButtonGroup;
import javax.swing.JRadioButton;

import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.openapi.project.Project;

/**
 * @author ven
 */
public abstract class JSBaseClassBasedIntroduceDialog extends JSBaseIntroduceDialog
{
	private static JSAttributeList.AccessType lastType;

	protected JSBaseClassBasedIntroduceDialog(final Project project, final JSExpression[] occurences, final JSExpression mainOccurence, String titleKey)
	{
		super(project, occurences, mainOccurence, titleKey);
	}

	@Override
	protected void doInit()
	{
		super.doInit();

		final ButtonGroup group = new ButtonGroup();
		group.add(getPrivateRadioButton());
		group.add(getPublicRadioButton());
		group.add(getPackageLocalRadioButton());
		group.add(getProtectedRadioButton());

		if(lastType == JSAttributeList.AccessType.PRIVATE || lastType == null)
		{
			getPrivateRadioButton().setSelected(true);
		}
		else if(lastType == JSAttributeList.AccessType.PROTECTED)
		{
			getProtectedRadioButton().setSelected(true);
		}
		else if(lastType == JSAttributeList.AccessType.PACKAGE_LOCAL)
		{
			getPackageLocalRadioButton().setSelected(true);
		}
		else if(lastType == JSAttributeList.AccessType.PUBLIC)
		{
			getPublicRadioButton().setSelected(true);
		}
	}

	public JSAttributeList.AccessType getAccessType()
	{
		JSAttributeList.AccessType type = null;
		if(getPublicRadioButton().isSelected())
		{
			type = JSAttributeList.AccessType.PUBLIC;
		}
		if(getPrivateRadioButton().isSelected())
		{
			type = JSAttributeList.AccessType.PRIVATE;
		}
		if(getPackageLocalRadioButton().isSelected())
		{
			type = JSAttributeList.AccessType.PACKAGE_LOCAL;
		}
		if(getProtectedRadioButton().isSelected())
		{
			type = JSAttributeList.AccessType.PROTECTED;
		}
		assert type != null;
		lastType = type;
		return type;
	}

	protected abstract JRadioButton getPrivateRadioButton();

	protected abstract JRadioButton getPublicRadioButton();

	protected abstract JRadioButton getProtectedRadioButton();

	protected abstract JRadioButton getPackageLocalRadioButton();
}
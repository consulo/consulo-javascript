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

package com.intellij.lang.javascript.inspections;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JavaScriptBundle;
import com.intellij.lang.javascript.psi.JSExpressionStatement;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.psi.PsiElement;
import com.intellij.ui.DocumentAdapter;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 9, 2008
 *         Time: 7:36:22 PM
 */
class CreateClassDialog extends DialogWrapper
{
	private JPanel myPanel;
	private JTextField myPackageName;
	private JLabel myClassName;

	protected CreateClassDialog(final Project project, String className, String packageName, boolean isInterface)
	{
		super(project, false);

		setTitle(JavaScriptBundle.message(isInterface ? "create.interface.dialog.title" : "create.class.dialog.title"));
		setModal(true);

		myPackageName.getDocument().addDocumentListener(new DocumentAdapter()
		{
			@Override
			protected void textChanged(final DocumentEvent e)
			{
				String text = getPackageName();
				boolean enabled;
				if(text.length() == 0)
				{
					enabled = true;
				}
				else
				{
					ASTNode node = JSChangeUtil.createJSTreeFromText(project, text);
					PsiElement elt;
					enabled = node != null &&
							(elt = node.getPsi()) instanceof JSExpressionStatement &&
							(elt = ((JSExpressionStatement) elt).getExpression()) instanceof JSReferenceExpression &&
							((JSReferenceExpression) elt).getReferencedName() != null &&
							elt.textMatches(text);
				}
				getOKAction().setEnabled(enabled);
			}
		});

		myClassName.setText(className);
		myPackageName.setText(packageName);

		init();
	}

	@Override
	protected JComponent createCenterPanel()
	{
		return myPanel;
	}

	@Override
	public JComponent getPreferredFocusedComponent()
	{
		return myPackageName;
	}

	String getPackageName()
	{
		return myPackageName.getText();
	}
}

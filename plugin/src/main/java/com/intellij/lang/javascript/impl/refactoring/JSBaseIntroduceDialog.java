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

package com.intellij.lang.javascript.impl.refactoring;

import consulo.javascript.language.JavaScriptBundle;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSEmbeddedContentImpl;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import consulo.application.Application;
import consulo.language.Language;
import consulo.language.editor.refactoring.NamesValidator;
import consulo.language.editor.refactoring.ui.ConflictsDialog;
import consulo.language.editor.refactoring.util.CommonRefactoringUtil;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.ResolveResult;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.project.Project;
import consulo.ui.ex.awt.DialogWrapper;
import consulo.ui.ex.awt.Messages;
import consulo.ui.ex.awt.util.Alarm;
import consulo.util.lang.ref.Ref;

import javax.swing.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

/**
 * @author ven
 */
public abstract class JSBaseIntroduceDialog extends DialogWrapper implements BaseIntroduceSettings
{
	private final Project myProject;
	private final JSExpression[] myOccurences;
	protected final JSExpression myMainOccurence;

	private Alarm myAlarm = new Alarm(Alarm.ThreadToUse.SWING_THREAD);

	protected JSBaseIntroduceDialog(final Project project, final JSExpression[] occurences, final JSExpression mainOccurence, String titleKey)
	{
		super(project, false);

		myProject = project;
		myOccurences = occurences;
		myMainOccurence = mainOccurence;
		setTitle(JavaScriptBundle.message(titleKey));
	}

	protected void doInit()
	{
		JCheckBox replaceAllCheckBox = getReplaceAllCheckBox();
		if(myOccurences.length > 1)
		{
			replaceAllCheckBox.setText(JavaScriptBundle.message("javascript.introduce.variable.replace.all.occurrences", myOccurences.length));
		}
		else
		{
			replaceAllCheckBox.setVisible(false);
		}

		final JTextField nameField = getNameField();
		nameField.setText(suggestCandidateName(myMainOccurence));
		nameField.selectAll();

		nameField.addKeyListener(new KeyAdapter()
		{
			@Override
			public void keyPressed(KeyEvent e)
			{
				initiateValidation();
			}
		});

		replaceAllCheckBox.setFocusable(false);

		final JComboBox typeField = getVarTypeField();

		final List<String> possibleTypes = new ArrayList<String>();
		final String type = JSResolveUtil.getExpressionType(myMainOccurence, myMainOccurence.getContainingFile());
		possibleTypes.add(type);

		typeField.setModel(new DefaultComboBoxModel(possibleTypes.toArray(new Object[possibleTypes.size()])));

		init();

		SwingUtilities.invokeLater(new Runnable()
		{
			@Override
			public void run()
			{
				initiateValidation();
			}
		});
	}

	protected String suggestCandidateName(JSExpression mainOccurence)
	{
		final String s = evaluateCandidate(mainOccurence);
		return s != null ? s.replace('.', '_') : null;
	}

	private static String evaluateCandidate(JSExpression mainOccurence)
	{
		if(mainOccurence instanceof JSCallExpression)
		{
			mainOccurence = ((JSCallExpression) mainOccurence).getMethodExpression();
		}

		if(mainOccurence instanceof JSReferenceExpression)
		{
			final ResolveResult[] results = ((JSReferenceExpression) mainOccurence).multiResolve(false);

			if(results.length > 0)
			{
				final PsiElement element = results[0].getElement();

				if(element instanceof JSFunction)
				{
					String typeString = ((JSFunction) element).getReturnTypeString();
					if(isValidIdentifier(typeString, mainOccurence))
					{
						return typeString;
					}
					return ((JSFunction) element).getName();
				}
				else if(element instanceof JSVariable)
				{
					String typeString = ((JSVariable) element).getTypeString();
					if(isValidIdentifier(typeString, mainOccurence))
					{
						return typeString;
					}
					return typeString;
				}
			}

			return ((JSReferenceExpression) mainOccurence).getReferencedName();
		}
		else if(mainOccurence.getParent() instanceof JSArgumentList)
		{
			JSParameter param = JSResolveUtil.findParameterForUsedArgument(mainOccurence, (JSArgumentList) mainOccurence.getParent());
			if(param != null)
			{
				return param.getName();
			}
		}

		return JSResolveUtil.getExpressionType(mainOccurence, mainOccurence.getContainingFile());
	}

	private static boolean isValidIdentifier(String typeString, PsiElement context)
	{
		if(typeString == null)
		{
			return false;
		}
		Language language = context.getContainingFile().getLanguage();
		return NamesValidator.forLanguage(language).isIdentifier(typeString, context.getProject());
	}

	protected abstract JTextField getNameField();

	protected abstract JPanel getPanel();

	protected abstract JCheckBox getReplaceAllCheckBox();

	private void initiateValidation()
	{
		myAlarm.cancelAllRequests();
		myAlarm.addRequest(new Runnable()
		{
			@Override
			public void run()
			{
				final String nameCandidate = getNameField().getText();
				setOKActionEnabled(nameCandidate.length() != 0 && isValidName(nameCandidate));
			}
		}, 100, Application.get().getCurrentModalityState());
	}

	@Override
	public JComponent getPreferredFocusedComponent()
	{
		return getNameField();
	}

	@Override
	protected JComponent createCenterPanel()
	{
		return getPanel();
	}

	@Override
	protected void doOKAction()
	{
		final String name = getVariableName();
		if(name.length() == 0 || !isValidName(name))
		{
			Messages.showErrorDialog(myProject, JavaScriptBundle.message("javascript.introduce.variable.invalid.name"),
					JavaScriptBundle.message("javascript.introduce.variable.title"));
			getNameField().requestFocus();
			return;
		}

		if(!checkConflicts(name))
		{
			return;
		}

		super.doOKAction();
	}

	private boolean checkConflicts(final String name)
	{
		PsiElement tmp = isReplaceAllOccurences() ? PsiTreeUtil.findCommonParent(myOccurences) : myMainOccurence;
		assert tmp != null;
		JSElement scope = PsiTreeUtil.getNonStrictParentOfType(tmp, JSBlockStatement.class, JSFile.class, JSEmbeddedContentImpl.class);
		assert scope != null;

		final Ref<JSNamedElement> existing = new Ref<JSNamedElement>();
		scope.accept(new JSElementVisitor()
		{
			@Override
			public void visitJSElement(final JSElement node)
			{
				if(existing.isNull())
				{
					node.acceptChildren(this);
				}
			}

			@Override
			public void visitJSVariable(final JSVariable node)
			{
				if(name.equals(node.getName()))
				{
					existing.set(node);
				}
				super.visitJSVariable(node);
			}

			@Override
			public void visitJSFunctionDeclaration(final JSFunction node)
			{
				if(name.equals(node.getName()))
				{
					existing.set(node);
				}
				super.visitJSFunctionDeclaration(node);
			}
		});

		if(existing.isNull())
		{
			final ResolveProcessor processor = new ResolveProcessor(name);
			JSResolveUtil.treeWalkUp(processor, scope, null, scope);
			final PsiElement resolved = processor.getResult();
			if(resolved instanceof JSNamedElement)
			{
				existing.set((JSNamedElement) resolved);
			}
		}

		if(!existing.isNull())
		{
			return showConflictsDialog(existing.get(), name);
		}

		return true;
	}

	private boolean showConflictsDialog(final JSNamedElement existing, final String name)
	{
		final String message = existing instanceof JSFunction ? JavaScriptBundle.message("javascript.introduce.variable.function.already.exists",
				CommonRefactoringUtil.htmlEmphasize(name)) : JavaScriptBundle.message("javascript.introduce.variable.variable.already.exists",
				CommonRefactoringUtil.htmlEmphasize(name));
		final ConflictsDialog conflictsDialog = new ConflictsDialog(myProject, message);
		conflictsDialog.show();
		return conflictsDialog.isOK();
	}

	@Override
	public boolean isReplaceAllOccurences()
	{
		return getReplaceAllCheckBox().isSelected();
	}

	@Override
	public String getVariableName()
	{
		return getNameField().getText().trim();
	}

	@Override
	public String getVariableType()
	{
		return (String) getVarTypeField().getSelectedItem();
	}

	private boolean isValidName(final String name)
	{
		final PsiFile containingFile = myMainOccurence.getContainingFile();
		return NamesValidator.forLanguage(containingFile.getLanguage()).isIdentifier(name, myProject);
	}

	public abstract JComboBox getVarTypeField();
}
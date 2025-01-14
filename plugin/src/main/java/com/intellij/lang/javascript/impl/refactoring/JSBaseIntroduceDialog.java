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

import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSEmbeddedContentImpl;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import consulo.annotation.access.RequiredReadAction;
import consulo.application.Application;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.Language;
import consulo.language.editor.refactoring.NamesValidator;
import consulo.language.editor.refactoring.ui.ConflictsDialog;
import consulo.language.editor.refactoring.util.CommonRefactoringUtil;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.ResolveResult;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import consulo.ui.annotation.RequiredUIAccess;
import consulo.ui.ex.awt.DialogWrapper;
import consulo.ui.ex.awt.Messages;
import consulo.ui.ex.awt.util.Alarm;
import consulo.util.lang.ref.SimpleReference;
import jakarta.annotation.Nonnull;

import javax.swing.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

/**
 * @author ven
 */
public abstract class JSBaseIntroduceDialog extends DialogWrapper implements BaseIntroduceSettings {
    private final Project myProject;
    private final JSExpression[] myOccurences;
    protected final JSExpression myMainOccurence;

    private Alarm myAlarm = new Alarm(Alarm.ThreadToUse.SWING_THREAD);

    @RequiredUIAccess
    protected JSBaseIntroduceDialog(
        Project project,
        JSExpression[] occurences,
        JSExpression mainOccurence,
        LocalizeValue title
    ) {
        super(project, false);

        myProject = project;
        myOccurences = occurences;
        myMainOccurence = mainOccurence;
        setTitle(title);
    }

    @RequiredReadAction
    protected void doInit() {
        JCheckBox replaceAllCheckBox = getReplaceAllCheckBox();
        if (myOccurences.length > 1) {
            replaceAllCheckBox.setText(JavaScriptLocalize.javascriptIntroduceVariableReplaceAllOccurrences(myOccurences.length).get());
        }
        else {
            replaceAllCheckBox.setVisible(false);
        }

        JTextField nameField = getNameField();
        nameField.setText(suggestCandidateName(myMainOccurence));
        nameField.selectAll();

        nameField.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                initiateValidation();
            }
        });

        replaceAllCheckBox.setFocusable(false);

        JComboBox typeField = getVarTypeField();

        List<String> possibleTypes = new ArrayList<>();
        String type = JSResolveUtil.getExpressionType(myMainOccurence, myMainOccurence.getContainingFile());
        possibleTypes.add(type);

        typeField.setModel(new DefaultComboBoxModel(possibleTypes.toArray(new Object[possibleTypes.size()])));

        init();

        SwingUtilities.invokeLater(this::initiateValidation);
    }

    @RequiredReadAction
    protected String suggestCandidateName(JSExpression mainOccurence) {
        String s = evaluateCandidate(mainOccurence);
        return s != null ? s.replace('.', '_') : null;
    }

    @RequiredReadAction
    private static String evaluateCandidate(JSExpression mainOccurence) {
        if (mainOccurence instanceof JSCallExpression call) {
            mainOccurence = call.getMethodExpression();
        }

        if (mainOccurence instanceof JSReferenceExpression refExpr) {
            ResolveResult[] results = refExpr.multiResolve(false);

            if (results.length > 0) {
                PsiElement element = results[0].getElement();

                if (element instanceof JSFunction function) {
                    String typeString = function.getReturnTypeString();
                    if (isValidIdentifier(typeString, mainOccurence)) {
                        return typeString;
                    }
                    return function.getName();
                }
                else if (element instanceof JSVariable variable) {
                    String typeString = variable.getTypeString();
                    if (isValidIdentifier(typeString, mainOccurence)) {
                        return typeString;
                    }
                    return typeString;
                }
            }

            return ((JSReferenceExpression)mainOccurence).getReferencedName();
        }
        else if (mainOccurence.getParent() instanceof JSArgumentList) {
            JSParameter param = JSResolveUtil.findParameterForUsedArgument(mainOccurence, (JSArgumentList)mainOccurence.getParent());
            if (param != null) {
                return param.getName();
            }
        }

        return JSResolveUtil.getExpressionType(mainOccurence, mainOccurence.getContainingFile());
    }

    @RequiredReadAction
    private static boolean isValidIdentifier(String typeString, PsiElement context) {
        if (typeString == null) {
            return false;
        }
        Language language = context.getContainingFile().getLanguage();
        return NamesValidator.forLanguage(language).isIdentifier(typeString, context.getProject());
    }

    protected abstract JTextField getNameField();

    protected abstract JPanel getPanel();

    protected abstract JCheckBox getReplaceAllCheckBox();

    private void initiateValidation() {
        myAlarm.cancelAllRequests();
        myAlarm.addRequest(
            () -> {
                final String nameCandidate = getNameField().getText();
                setOKActionEnabled(nameCandidate.length() != 0 && isValidName(nameCandidate));
            },
            100,
            Application.get().getCurrentModalityState()
        );
    }

    @Override
    @RequiredUIAccess
    public JComponent getPreferredFocusedComponent() {
        return getNameField();
    }

    @Override
    protected JComponent createCenterPanel() {
        return getPanel();
    }

    @Override
    @RequiredUIAccess
    protected void doOKAction() {
        String name = getVariableName();
        if (name.length() == 0 || !isValidName(name)) {
            Messages.showErrorDialog(
                myProject,
                JavaScriptLocalize.javascriptIntroduceVariableInvalidName().get(),
                JavaScriptLocalize.javascriptIntroduceVariableTitle().get()
            );
            getNameField().requestFocus();
            return;
        }

        if (!checkConflicts(name)) {
            return;
        }

        super.doOKAction();
    }

    @RequiredUIAccess
    private boolean checkConflicts(String name) {
        PsiElement tmp = isReplaceAllOccurences() ? PsiTreeUtil.findCommonParent(myOccurences) : myMainOccurence;
        assert tmp != null;
        JSElement scope = PsiTreeUtil.getNonStrictParentOfType(tmp, JSBlockStatement.class, JSFile.class, JSEmbeddedContentImpl.class);
        assert scope != null;

        final SimpleReference<JSNamedElement> existing = new SimpleReference<>();
        scope.accept(new JSElementVisitor() {
            @Override
            public void visitJSElement(@Nonnull JSElement node) {
                if (existing.isNull()) {
                    node.acceptChildren(this);
                }
            }

            @Override
            @RequiredReadAction
            public void visitJSVariable(@Nonnull JSVariable node) {
                if (name.equals(node.getName())) {
                    existing.set(node);
                }
                super.visitJSVariable(node);
            }

            @Override
            @RequiredReadAction
            public void visitJSFunctionDeclaration(@Nonnull JSFunction node) {
                if (name.equals(node.getName())) {
                    existing.set(node);
                }
                super.visitJSFunctionDeclaration(node);
            }
        });

        if (existing.isNull()) {
            ResolveProcessor processor = new ResolveProcessor(name);
            JSResolveUtil.treeWalkUp(processor, scope, null, scope);
            PsiElement resolved = processor.getResult();
            if (resolved instanceof JSNamedElement namedElement) {
                existing.set(namedElement);
            }
        }

        return existing.isNull() || showConflictsDialog(existing.get(), name);
    }

    @RequiredUIAccess
    private boolean showConflictsDialog(JSNamedElement existing, String name) {
        LocalizeValue message = existing instanceof JSFunction
            ? JavaScriptLocalize.javascriptIntroduceVariableFunctionAlreadyExists(CommonRefactoringUtil.htmlEmphasize(name))
            : JavaScriptLocalize.javascriptIntroduceVariableVariableAlreadyExists(CommonRefactoringUtil.htmlEmphasize(name));
        ConflictsDialog conflictsDialog = new ConflictsDialog(myProject, message.get());
        conflictsDialog.show();
        return conflictsDialog.isOK();
    }

    @Override
    public boolean isReplaceAllOccurences() {
        return getReplaceAllCheckBox().isSelected();
    }

    @Override
    public String getVariableName() {
        return getNameField().getText().trim();
    }

    @Override
    public String getVariableType() {
        return (String)getVarTypeField().getSelectedItem();
    }

    private boolean isValidName(String name) {
        PsiFile containingFile = myMainOccurence.getContainingFile();
        return NamesValidator.forLanguage(containingFile.getLanguage()).isIdentifier(name, myProject);
    }

    public abstract JComboBox getVarTypeField();
}
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

package com.intellij.lang.javascript.impl.refactoring.introduceField;

import com.intellij.lang.javascript.formatter.JSCodeStyleSettings;
import com.intellij.lang.javascript.impl.refactoring.JSBaseClassBasedIntroduceDialog;
import com.intellij.lang.javascript.psi.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.codeStyle.CodeStyleSettingsManager;
import consulo.language.psi.PsiElement;
import consulo.language.psi.ResolveResult;
import consulo.project.Project;
import consulo.ui.annotation.RequiredUIAccess;
import consulo.util.lang.StringUtil;
import consulo.util.lang.ref.SimpleReference;
import jakarta.annotation.Nonnull;

import javax.swing.*;

/**
 * @author Maxim.Mossienko
 * @author 2008-07-24
 */
class JSIntroduceFieldDialog extends JSBaseClassBasedIntroduceDialog implements JSIntroduceFieldSettings {
    private JCheckBox myReplaceAllCheckBox;
    private JTextField myNameField;
    private JPanel myPanel;
    private JRadioButton myPublic;
    private JRadioButton myPackageLocal;
    private JRadioButton myProtected;
    private JRadioButton myPrivate;
    private JComboBox myVarType;

    private JRadioButton myFieldDeclaration;
    private JRadioButton myCurrentMethod;
    private JRadioButton myConstructor;

    private static InitializationPlace lastInitializationPlace = InitializationPlace.FieldDeclaration;

    @RequiredUIAccess
    public JSIntroduceFieldDialog(Project project, JSExpression[] occurrences, JSExpression expression) {
        super(project, occurrences, expression, JavaScriptLocalize.javascriptIntroduceFieldTitle());
        doInit();
    }

    @Override
    @RequiredReadAction
    protected void doInit() {
        super.doInit();

        final SimpleReference<Boolean> localContextDependent = new SimpleReference<>();
        myMainOccurrence.accept(new JSElementVisitor() {
            @Override
            @RequiredReadAction
            public void visitJSReferenceExpression(@Nonnull JSReferenceExpression node) {
                if (node.getQualifier() == null) {
                    ResolveResult[] results = node.multiResolve(true);
                    if (results.length == 0) {
                        localContextDependent.set(Boolean.TRUE);
                    }
                    else {
                        PsiElement element = results[0].getElement();
                        if (element instanceof JSVariable variable && !(variable.getParent().getParent() instanceof JSClass)) {
                            localContextDependent.set(Boolean.TRUE);
                        }
                    }
                }
                super.visitJSReferenceExpression(node);
            }

            @Override
            public void visitJSElement(@Nonnull JSElement node) {
                node.acceptChildren(this);
            }
        });

        if (Boolean.TRUE.equals(localContextDependent.get())) {
            myConstructor.setEnabled(false);
            myFieldDeclaration.setEnabled(false);
            myCurrentMethod.setSelected(true);
        }
        else if (lastInitializationPlace == InitializationPlace.Constructor) {
            myConstructor.setSelected(true);
        }
        else if (lastInitializationPlace == InitializationPlace.CurrentMethod) {
            myCurrentMethod.setSelected(true);
        }
        else {
            myFieldDeclaration.setSelected(true);
        }
    }

    @Override
    protected JTextField getNameField() {
        return myNameField;
    }

    @Override
    protected JPanel getPanel() {
        return myPanel;
    }

    @Override
    protected JCheckBox getReplaceAllCheckBox() {
        return myReplaceAllCheckBox;
    }

    @Override
    public JComboBox getVarTypeField() {
        return myVarType;
    }

    @Override
    protected JRadioButton getPrivateRadioButton() {
        return myPrivate;
    }

    @Override
    protected JRadioButton getPublicRadioButton() {
        return myPublic;
    }

    @Override
    protected JRadioButton getProtectedRadioButton() {
        return myProtected;
    }

    @Override
    protected JRadioButton getPackageLocalRadioButton() {
        return myPackageLocal;
    }

    @Override
    public InitializationPlace getInitializationPlace() {
        return myFieldDeclaration.isSelected() ? InitializationPlace.FieldDeclaration : myCurrentMethod.isSelected() ? InitializationPlace.CurrentMethod :
            InitializationPlace.Constructor;
    }

    @Override
    @RequiredUIAccess
    protected void doOKAction() {
        super.doOKAction();
        lastInitializationPlace = getInitializationPlace();
    }

    @Override
    @RequiredReadAction
    protected String suggestCandidateName(JSExpression mainOccurrence) {
        String s = super.suggestCandidateName(mainOccurrence);
        JSCodeStyleSettings jsCodeStyleSettings = CodeStyleSettingsManager.getSettings(mainOccurrence.getProject())
            .getCustomSettings(JSCodeStyleSettings.class);
        if (jsCodeStyleSettings.FIELD_PREFIX.length() > 0) {
            return jsCodeStyleSettings.FIELD_PREFIX + s;
        }
        if (s.length() > 0) {
            return StringUtil.decapitalize(s);
        }
        return s;
    }
}

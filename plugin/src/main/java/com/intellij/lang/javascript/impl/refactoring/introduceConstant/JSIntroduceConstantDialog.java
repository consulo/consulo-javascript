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

package com.intellij.lang.javascript.impl.refactoring.introduceConstant;

import com.intellij.lang.javascript.impl.refactoring.JSBaseClassBasedIntroduceDialog;
import com.intellij.lang.javascript.psi.JSExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.project.Project;

import javax.swing.*;

/**
 * @author Maxim.Mossienko
 * @since 2008-07-24
 */
class JSIntroduceConstantDialog extends JSBaseClassBasedIntroduceDialog implements JSIntroduceConstantSettings {
    private JTextField myNameField;
    private JCheckBox myReplaceAllCheckBox;
    private JPanel myPanel;
    private JRadioButton myPrivate;
    private JRadioButton myProtected;
    private JRadioButton myPackageLocal;
    private JRadioButton myPublic;
    private JComboBox myVarType;

    protected JSIntroduceConstantDialog(Project project, JSExpression[] occurences, JSExpression mainOccurence) {
        super(project, occurences, mainOccurence, JavaScriptLocalize.javascriptIntroduceConstantTitle());

        doInit();
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
    @RequiredReadAction
    protected String suggestCandidateName(JSExpression mainOccurence) {
        return super.suggestCandidateName(mainOccurence).toUpperCase();
    }
}

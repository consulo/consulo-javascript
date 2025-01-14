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

import javax.swing.ButtonGroup;
import javax.swing.JRadioButton;

import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import consulo.ui.annotation.RequiredUIAccess;

/**
 * @author ven
 */
public abstract class JSBaseClassBasedIntroduceDialog extends JSBaseIntroduceDialog {
    private static JSAttributeList.AccessType lastType;

    @RequiredUIAccess
    protected JSBaseClassBasedIntroduceDialog(Project project, JSExpression[] occurences, JSExpression mainOccurence, LocalizeValue title) {
        super(project, occurences, mainOccurence, title);
    }

    @Override
    @RequiredReadAction
    protected void doInit() {
        super.doInit();

        ButtonGroup group = new ButtonGroup();
        group.add(getPrivateRadioButton());
        group.add(getPublicRadioButton());
        group.add(getPackageLocalRadioButton());
        group.add(getProtectedRadioButton());

        if (lastType == JSAttributeList.AccessType.PRIVATE || lastType == null) {
            getPrivateRadioButton().setSelected(true);
        }
        else if (lastType == JSAttributeList.AccessType.PROTECTED) {
            getProtectedRadioButton().setSelected(true);
        }
        else if (lastType == JSAttributeList.AccessType.PACKAGE_LOCAL) {
            getPackageLocalRadioButton().setSelected(true);
        }
        else if (lastType == JSAttributeList.AccessType.PUBLIC) {
            getPublicRadioButton().setSelected(true);
        }
    }

    public JSAttributeList.AccessType getAccessType() {
        JSAttributeList.AccessType type = null;
        if (getPublicRadioButton().isSelected()) {
            type = JSAttributeList.AccessType.PUBLIC;
        }
        if (getPrivateRadioButton().isSelected()) {
            type = JSAttributeList.AccessType.PRIVATE;
        }
        if (getPackageLocalRadioButton().isSelected()) {
            type = JSAttributeList.AccessType.PACKAGE_LOCAL;
        }
        if (getProtectedRadioButton().isSelected()) {
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
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

package com.intellij.lang.javascript.impl.inspections;

import com.intellij.lang.javascript.psi.JSExpressionStatement;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.ast.ASTNode;
import consulo.project.Project;
import consulo.ui.annotation.RequiredUIAccess;
import consulo.ui.ex.awt.DialogWrapper;
import consulo.ui.ex.awt.event.DocumentAdapter;

import javax.swing.*;
import javax.swing.event.DocumentEvent;

/**
 * @author Maxim.Mossienko
 * @since 2008-06-09
 */
class CreateClassDialog extends DialogWrapper {
    private JPanel myPanel;
    private JTextField myPackageName;
    private JLabel myClassName;

    protected CreateClassDialog(final Project project, String className, String packageName, boolean isInterface) {
        super(project, false);

        setTitle(isInterface ? JavaScriptLocalize.createInterfaceDialogTitle() : JavaScriptLocalize.createClassDialogTitle());
        setModal(true);

        myPackageName.getDocument().addDocumentListener(new DocumentAdapter() {
            @Override
            @RequiredReadAction
            protected void textChanged(DocumentEvent e) {
                String text = getPackageName();
                boolean enabled;
                if (text.length() == 0) {
                    enabled = true;
                }
                else {
                    ASTNode node = JSChangeUtil.createJSTreeFromText(project, text);
                    enabled = node != null
                        && node.getPsi() instanceof JSExpressionStatement expressionStatement
                        && expressionStatement.getExpression() instanceof JSReferenceExpression refExpr
                        && refExpr.getReferencedName() != null
                        && refExpr.textMatches(text);
                }
                getOKAction().setEnabled(enabled);
            }
        });

        myClassName.setText(className);
        myPackageName.setText(packageName);

        init();
    }

    @Override
    protected JComponent createCenterPanel() {
        return myPanel;
    }

    @Override
    @RequiredUIAccess
    public JComponent getPreferredFocusedComponent() {
        return myPackageName;
    }

    String getPackageName() {
        return myPackageName.getText();
    }
}

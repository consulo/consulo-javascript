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

package com.intellij.lang.javascript.impl.formatter;

import com.intellij.lang.javascript.formatter.JSCodeStyleSettings;
import consulo.codeEditor.EditorHighlighter;
import consulo.colorScheme.EditorColorsScheme;
import consulo.javascript.language.JavaScriptFileType;
import consulo.language.codeStyle.CodeStyleSettings;
import consulo.language.codeStyle.ui.setting.CodeStyleAbstractPanel;
import consulo.language.editor.highlight.EditorHighlighterFactory;
import consulo.language.file.light.LightVirtualFile;
import consulo.language.psi.PsiFile;
import consulo.ui.annotation.RequiredUIAccess;
import consulo.ui.ex.awt.event.DocumentAdapter;
import consulo.virtualFileSystem.fileType.FileType;
import jakarta.annotation.Nonnull;

import javax.swing.*;
import javax.swing.event.DocumentEvent;

/**
 * @author Maxim.Mossienko
 * @since 2008-03-12
 */
public class JSCodeStylePanel extends CodeStyleAbstractPanel {
    private JPanel myPanel;
    private JCheckBox myIndentPackageChildren;
    private JPanel myPreviewPanel;
    private JTextField myFieldPrefixTextField;
    private JTextField myPropertyPrefixTextField;
    private JCheckBox myUseSemicolon;
    private boolean myInsideUpdate = false;

    @RequiredUIAccess
    public JSCodeStylePanel(CodeStyleSettings settings) {
        super(settings);

        installPreviewPanel(myPreviewPanel);
        addPanelToWatch(myPanel);

        myUseSemicolon.addItemListener(e -> {
            if (!myInsideUpdate) {
                //updatePreviewEditor();
                somethingChanged();
            }
        });

        DocumentAdapter adapter = new DocumentAdapter() {
            @Override
            protected void textChanged(DocumentEvent e) {
                if (!myInsideUpdate) {
                    //updatePreviewEditor();
                    somethingChanged();
                }
            }
        };

        myFieldPrefixTextField.getDocument().addDocumentListener(adapter);
        myPropertyPrefixTextField.getDocument().addDocumentListener(adapter);
    }

    @Override
    protected EditorHighlighter createHighlighter(EditorColorsScheme scheme) {
        return EditorHighlighterFactory.getInstance().createEditorHighlighter(new LightVirtualFile("a.as"), scheme, null);
    }

    @Override
    protected int getRightMargin() {
        return 60;
    }

    @Override
    protected void prepareForReformat(PsiFile psiFile) {
    }

    @Override
    @Nonnull
    protected FileType getFileType() {
        return JavaScriptFileType.INSTANCE;
    }

    @Override
    protected String getPreviewText() {
        JSCodeStyleSettings jsCodeStyleSettings = getSettings().getCustomSettings(JSCodeStyleSettings.class);
        String baseName = "field";
        String propertyName =
            (myPropertyPrefixTextField != null ? myPropertyPrefixTextField.getText() : jsCodeStyleSettings.PROPERTY_PREFIX) +
                baseName;
        String varName =
            (myFieldPrefixTextField != null ? myFieldPrefixTextField.getText() : jsCodeStyleSettings.FIELD_PREFIX) + baseName;
        String semiColon =
            (myUseSemicolon != null ? myUseSemicolon.isSelected() : jsCodeStyleSettings.USE_SEMICOLON_AFTER_STATEMENT) ? ";" : "";

        return "package aaa {\n" +
            "class XXX {\n" +
            "private var " + varName + semiColon + "\n" +
            "function get " + propertyName + "() {\n" +
            "return " + varName + semiColon + "\n" +
            "}\n}\n}";
    }

    @Override
    public void apply(CodeStyleSettings settings) {
        JSCodeStyleSettings jsCodeStyleSettings = settings.getCustomSettings(JSCodeStyleSettings.class);
        jsCodeStyleSettings.INDENT_PACKAGE_CHILDREN = myIndentPackageChildren.isSelected()
            ? JSCodeStyleSettings.INDENT
            : JSCodeStyleSettings.DO_NOT_INDENT;
        jsCodeStyleSettings.FIELD_PREFIX = myFieldPrefixTextField.getText();
        jsCodeStyleSettings.PROPERTY_PREFIX = myPropertyPrefixTextField.getText();
        jsCodeStyleSettings.USE_SEMICOLON_AFTER_STATEMENT = myUseSemicolon.isSelected();
    }

    @Override
    public boolean isModified(CodeStyleSettings settings) {
        JSCodeStyleSettings jsCodeStyleSettings = settings.getCustomSettings(JSCodeStyleSettings.class);
        return (jsCodeStyleSettings.INDENT_PACKAGE_CHILDREN == JSCodeStyleSettings.INDENT) != myIndentPackageChildren.isSelected()
            || !jsCodeStyleSettings.FIELD_PREFIX.equals(myFieldPrefixTextField.getText())
            || !jsCodeStyleSettings.PROPERTY_PREFIX.equals(myPropertyPrefixTextField.getText())
            || jsCodeStyleSettings.USE_SEMICOLON_AFTER_STATEMENT != (myUseSemicolon.isSelected());
    }

    @Override
    public JComponent getPanel() {
        return myPanel;
    }

    @Override
    protected void resetImpl(CodeStyleSettings settings) {
        try {
            myInsideUpdate = true;
            JSCodeStyleSettings jsCodeStyleSettings = settings.getCustomSettings(JSCodeStyleSettings.class);
            myIndentPackageChildren.setSelected(jsCodeStyleSettings.INDENT_PACKAGE_CHILDREN == JSCodeStyleSettings.INDENT);
            myFieldPrefixTextField.setText(jsCodeStyleSettings.FIELD_PREFIX);
            myPropertyPrefixTextField.setText(jsCodeStyleSettings.PROPERTY_PREFIX);
            myUseSemicolon.setSelected(jsCodeStyleSettings.USE_SEMICOLON_AFTER_STATEMENT);
        }
        finally {
            myInsideUpdate = false;
        }
    }

    @Override
    protected String getFileTypeExtension(FileType fileType) {
        return "js2";
    }
}

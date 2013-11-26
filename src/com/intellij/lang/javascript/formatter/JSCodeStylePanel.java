package com.intellij.lang.javascript.formatter;

import com.intellij.application.options.CodeStyleAbstractPanel;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.openapi.editor.colors.EditorColorsScheme;
import com.intellij.openapi.editor.highlighter.EditorHighlighter;
import com.intellij.openapi.editor.highlighter.EditorHighlighterFactory;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.PsiFile;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.testFramework.LightVirtualFile;
import com.intellij.ui.DocumentAdapter;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

/**
 * @author Maxim.Mossienko
 * Date: Mar 12, 2008
 * Time: 10:39:21 PM
 */
public class JSCodeStylePanel extends CodeStyleAbstractPanel {
  private JPanel myPanel;
  private JCheckBox myIndentPackageChildren;
  private JPanel myPreviewPanel;
  private JTextField myFieldPrefixTextField;
  private JTextField myPropertyPrefixTextField;
  private JCheckBox myUseSemicolon;
  private static CodeStyleSettings mySettings;
  private boolean myInsideUpdate = false;

  public JSCodeStylePanel(final CodeStyleSettings settings) {
    super(mySettings = settings);

    installPreviewPanel(myPreviewPanel);
    addPanelToWatch(myPanel);

    myUseSemicolon.addItemListener(new ItemListener() {
      public void itemStateChanged(final ItemEvent e) {
        if (!myInsideUpdate) {
          //updatePreviewEditor();
          somethingChanged();
        }
      }
    });

    final DocumentAdapter adapter = new DocumentAdapter() {
      protected void textChanged(final DocumentEvent e) {
        if (!myInsideUpdate) {
          //updatePreviewEditor();
          somethingChanged();
        }
      }
    };

    myFieldPrefixTextField.getDocument().addDocumentListener(adapter);
    myPropertyPrefixTextField.getDocument().addDocumentListener(adapter);
  }

  protected EditorHighlighter createHighlighter(final EditorColorsScheme scheme) {
    return EditorHighlighterFactory.getInstance().createEditorHighlighter(new LightVirtualFile("a.as"), scheme, null);
  }

  protected int getRightMargin() {
    return 60;
  }

  protected void prepareForReformat(final PsiFile psiFile) {
  }

  @NotNull
  protected FileType getFileType() {
    return JavaScriptSupportLoader.JAVASCRIPT;
  }

  protected String getPreviewText() {
    final JSCodeStyleSettings jsCodeStyleSettings = mySettings.getCustomSettings(JSCodeStyleSettings.class);
    @NonNls String baseName = "field";
    @NonNls String propertyName = (myPropertyPrefixTextField != null ? myPropertyPrefixTextField.getText() : jsCodeStyleSettings.PROPERTY_PREFIX) + baseName;
    @NonNls String varName = (myFieldPrefixTextField != null ? myFieldPrefixTextField.getText() : jsCodeStyleSettings.FIELD_PREFIX) + baseName;
    @NonNls String semiColon = (myUseSemicolon != null ? myUseSemicolon.isSelected() : jsCodeStyleSettings.USE_SEMICOLON_AFTER_STATEMENT) ? ";":"";
    
    return "package aaa {\nclass XXX {\n" +
           "private var " + varName + semiColon + "\n" +
           "function get " +  propertyName + "() {\nreturn " + varName +  semiColon + "\n" +
           "}\n}\n}";
  }

  public void apply(final CodeStyleSettings settings) {
    final JSCodeStyleSettings jsCodeStyleSettings = settings.getCustomSettings(JSCodeStyleSettings.class);
    jsCodeStyleSettings.INDENT_PACKAGE_CHILDREN = myIndentPackageChildren.isSelected() ? JSCodeStyleSettings.INDENT: JSCodeStyleSettings.DO_NOT_INDENT;
    jsCodeStyleSettings.FIELD_PREFIX = myFieldPrefixTextField.getText();
    jsCodeStyleSettings.PROPERTY_PREFIX = myPropertyPrefixTextField.getText();
    jsCodeStyleSettings.USE_SEMICOLON_AFTER_STATEMENT = myUseSemicolon.isSelected();
  }

  public boolean isModified(final CodeStyleSettings settings) {
    final JSCodeStyleSettings jsCodeStyleSettings = settings.getCustomSettings(JSCodeStyleSettings.class);
    return (jsCodeStyleSettings.INDENT_PACKAGE_CHILDREN == JSCodeStyleSettings.INDENT) != myIndentPackageChildren.isSelected() ||
        !jsCodeStyleSettings.FIELD_PREFIX.equals(myFieldPrefixTextField.getText()) ||
        !jsCodeStyleSettings.PROPERTY_PREFIX.equals(myPropertyPrefixTextField.getText()) ||
        jsCodeStyleSettings.USE_SEMICOLON_AFTER_STATEMENT != (myUseSemicolon.isSelected());
  }

  public JComponent getPanel() {
    return myPanel;
  }

  protected void resetImpl(final CodeStyleSettings settings) {
    try {
      myInsideUpdate = true;
      final JSCodeStyleSettings jsCodeStyleSettings = settings.getCustomSettings(JSCodeStyleSettings.class);
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
  protected String getFileTypeExtension(final FileType fileType) {
    return "js2";
  }
}

package com.intellij.lang.javascript.refactoring.extractMethod;

import com.intellij.lang.javascript.JSBundle;
import com.intellij.openapi.ui.DialogWrapper;

import javax.swing.*;

/**
 * @author Maxim.Mossienko
 *         Date: Aug 9, 2008
 *         Time: 9:22:10 AM
 */
public class JSExtractFunctionDialog extends DialogWrapper implements JSExtractFunctionSettings {
  private JPanel myPanel;
  private JTextField myFunctionName;

  JSExtractFunctionDialog() {
    super(false);
    
    setTitle(JSBundle.message("javascript.extract.method.title"));
    init();
  }

  protected JComponent createCenterPanel() {
    return myPanel;
  }

  public String getMethodName() {
    return myFunctionName.getText();
  }
}

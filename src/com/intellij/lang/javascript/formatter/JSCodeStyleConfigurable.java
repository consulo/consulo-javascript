package com.intellij.lang.javascript.formatter;

import com.intellij.application.options.CodeStyleAbstractConfigurable;
import com.intellij.application.options.CodeStyleAbstractPanel;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.psi.codeStyle.CodeStyleSettings;

import javax.swing.*;

/**
 * Created by IntelliJ IDEA.
 * User: Maxim.Mossienko
 * Date: Mar 12, 2008
 * Time: 10:36:16 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSCodeStyleConfigurable extends CodeStyleAbstractConfigurable {
  public JSCodeStyleConfigurable(final CodeStyleSettings settings, final CodeStyleSettings codeStyleSettings) {
    super(settings, codeStyleSettings, JSBundle.message("js.code.style.tab.name"));
  }

  protected CodeStyleAbstractPanel createPanel(final CodeStyleSettings settings) {
    return new JSCodeStylePanel(settings);
  }

  public String getHelpTopic() {
    return null; // TODO
  }

  @Override
  public Icon getIcon() {
    return JavaScriptSupportLoader.JAVASCRIPT.getIcon();
  }
}

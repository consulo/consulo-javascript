package com.intellij.lang.javascript.inspections;

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.psi.JSExpressionStatement;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.psi.PsiElement;
import com.intellij.ui.DocumentAdapter;

import javax.swing.*;
import javax.swing.event.DocumentEvent;

/**
 * @author Maxim.Mossienko
*         Date: Jun 9, 2008
*         Time: 7:36:22 PM
*/
class CreateClassDialog extends DialogWrapper {
  private JPanel myPanel;
  private JTextField myPackageName;
  private JLabel myClassName;

  protected CreateClassDialog(final Project project, String className, String packageName, boolean isInterface) {
    super(project, false);

    setTitle(JSBundle.message(isInterface ? "create.interface.dialog.title":"create.class.dialog.title"));
    setModal(true);

    myPackageName.getDocument().addDocumentListener(new DocumentAdapter() {
      protected void textChanged(final DocumentEvent e) {
        String text = getPackageName();
        boolean enabled;
        if (text.length() == 0) {
          enabled = true;
        } else {
          ASTNode node = JSChangeUtil.createJSTreeFromText(project, text);
          PsiElement elt;
          enabled = node != null &&
                    (elt = node.getPsi()) instanceof JSExpressionStatement &&
                    (elt = ((JSExpressionStatement)elt).getExpression()) instanceof JSReferenceExpression &&
                    ((JSReferenceExpression)elt).getReferencedName() != null &&
                    elt.textMatches(text);
        }
        getOKAction().setEnabled(enabled);
      }
    });
    
    myClassName.setText(className);
    myPackageName.setText(packageName);

    init();
  }

  protected JComponent createCenterPanel() {
    return myPanel;
  }

  @Override
  public JComponent getPreferredFocusedComponent() {
    return myPackageName;
  }

  String getPackageName() {
    return myPackageName.getText();
  }
}

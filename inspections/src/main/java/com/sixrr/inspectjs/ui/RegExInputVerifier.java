package com.sixrr.inspectjs.ui;

import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.ui.ex.awt.Messages;

import javax.swing.*;
import java.text.ParseException;

public class RegExInputVerifier extends InputVerifier {
    @Override
    public boolean verify(JComponent input) {
        return true;
    }

    @Override
    public boolean shouldYieldFocus(JComponent input) {
        if (input instanceof JFormattedTextField ftf) {
            JFormattedTextField.AbstractFormatter formatter = ftf.getFormatter();
            if (formatter != null) {
                try {
                    formatter.stringToValue(ftf.getText());
                }
                catch (ParseException e) {
                    SwingUtilities.invokeLater(
                        () -> Messages.showErrorDialog(e.getMessage(), InspectionJSLocalize.malformedNamingPatternAlert().get())
                    );
                }
            }
        }
        return true;
    }
}
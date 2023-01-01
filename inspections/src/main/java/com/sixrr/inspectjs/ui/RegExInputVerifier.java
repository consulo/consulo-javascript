package com.sixrr.inspectjs.ui;

import consulo.ui.ex.awt.Messages;
import com.sixrr.inspectjs.InspectionJSBundle;

import javax.swing.*;
import java.text.ParseException;

public class RegExInputVerifier extends InputVerifier {
    @Override
	public boolean verify(JComponent input) {
        return true;
    }

    @Override
	public boolean shouldYieldFocus(JComponent input) {
        if (input instanceof JFormattedTextField) {
            final JFormattedTextField ftf = (JFormattedTextField) input;
            final JFormattedTextField.AbstractFormatter formatter = ftf.getFormatter();
            if (formatter != null) {
                try {
                    formatter.stringToValue(ftf.getText());
                } catch (final ParseException e) {
                    SwingUtilities.invokeLater(new Runnable() {
                        @Override
						public void run() {
                            Messages.showErrorDialog(e.getMessage(), InspectionJSBundle.message("malformed.naming.pattern.alert"));
                        }
                    });
                }
            }
        }
        return true;
    }
}
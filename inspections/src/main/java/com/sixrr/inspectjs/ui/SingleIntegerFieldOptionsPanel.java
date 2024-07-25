package com.sixrr.inspectjs.ui;

import com.sixrr.inspectjs.BaseInspection;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.Document;
import java.awt.*;
import java.text.NumberFormat;

import org.jetbrains.annotations.NonNls;

public class SingleIntegerFieldOptionsPanel extends JPanel {
    public SingleIntegerFieldOptionsPanel(String labelString, final BaseInspection owner, @NonNls final String property) {
        super(new GridBagLayout());
        final JLabel label = new JLabel(labelString);
        final NumberFormat formatter = NumberFormat.getIntegerInstance();
        formatter.setParseIntegerOnly(true);
        final JFormattedTextField valueField = new JFormattedTextField(formatter);
        valueField.setValue(getPropertyValue(owner, property));
        valueField.setColumns(2);
        final Document document = valueField.getDocument();
        document.addDocumentListener(new DocumentListener() {
            @Override
            public void changedUpdate(DocumentEvent e) {
                textChanged();
            }

            @Override
            public void insertUpdate(DocumentEvent e) {
                textChanged();
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                textChanged();
            }

            private void textChanged() {
                setPropertyValue(owner, property, ((Number)valueField.getValue()).intValue());
            }
        });
        final GridBagConstraints constraints = new GridBagConstraints();
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 1.0;
        constraints.anchor = GridBagConstraints.EAST;
        constraints.fill = GridBagConstraints.NONE;
        add(label, constraints);
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.weightx = 1.0;
        constraints.anchor = GridBagConstraints.WEST;
        constraints.fill = GridBagConstraints.NONE;
        add(valueField, constraints);
    }

    private void setPropertyValue(BaseInspection owner, String property, int value) {
        try {
            owner.getClass().getField(property).setInt(owner, value);
        }
        catch (Exception ignore) {
        }
    }

    private int getPropertyValue(BaseInspection owner, String property) {
        try {
            return owner.getClass().getField(property).getInt(owner);
        }
        catch (Exception e) {
            return 0;
        }
    }
}

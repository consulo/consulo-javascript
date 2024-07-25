package com.sixrr.inspectjs.ui;

import com.sixrr.inspectjs.BaseInspection;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.awt.*;

import org.jetbrains.annotations.NonNls;

public class SingleCheckboxOptionsPanel extends JPanel {
    public SingleCheckboxOptionsPanel(String label, final BaseInspection owner, @NonNls final String property) {
        super(new GridBagLayout());
        final boolean selected = getPropertyValue(owner, property);
        final JCheckBox checkBox = new JCheckBox(label, selected);
        final ButtonModel model = checkBox.getModel();
        model.addChangeListener(e -> setPropertyValue(owner, property, model.isSelected()));

        final GridBagConstraints constraints = new GridBagConstraints();
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 1.0;
        constraints.anchor = GridBagConstraints.CENTER;
        constraints.fill = GridBagConstraints.HORIZONTAL;
        add(checkBox, constraints);
    }

    private void setPropertyValue(BaseInspection owner, String property, boolean selected) {
        try {
            owner.getClass().getField(property).setBoolean(owner, selected);
        }
        catch (Exception ignore) {
        }
    }

    private boolean getPropertyValue(BaseInspection owner, String property) {
        try {
            return owner.getClass().getField(property).getBoolean(owner);
        }
        catch (Exception e) {
            return false;
        }
    }
}

package com.sixrr.inspectjs.ui;

import org.jetbrains.annotations.NonNls;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;

public class FormattedTextFieldMacFix {
    private FormattedTextFieldMacFix() {
        super();
    }

    public static void apply(JFormattedTextField field) {
        if (isMacOs()) {
            final Toolkit toolkit = Toolkit.getDefaultToolkit();
            final int commandKeyMask = toolkit.getMenuShortcutKeyMask();
            @NonNls final InputMap inputMap = field.getInputMap();
            final KeyStroke copyKeyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_C, commandKeyMask);
            inputMap.put(copyKeyStroke, "copy-to-clipboard");
            final KeyStroke pasteKeyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_V, commandKeyMask);
            inputMap.put(pasteKeyStroke, "paste-from-clipboard");
            final KeyStroke cutKeyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_X, commandKeyMask);
            inputMap.put(cutKeyStroke, "cut-to-clipboard");
        }
    }

    @SuppressWarnings({"HardCodedStringLiteral"})
    private static boolean isMacOs() {
        final String osName = System.getProperty("os.name").toLowerCase();
        if (osName == null) {
            return false;
        }
        return osName.startsWith("mac os x");
    }
}

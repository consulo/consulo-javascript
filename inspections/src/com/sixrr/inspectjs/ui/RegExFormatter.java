package com.sixrr.inspectjs.ui;

import javax.swing.text.DefaultFormatter;
import java.text.ParseException;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

public class RegExFormatter extends DefaultFormatter {
    public RegExFormatter() {
        super();
        setOverwriteMode(false);
    }

    public Object stringToValue(String text) throws ParseException {
        try {
            return Pattern.compile(text);
        } catch (final PatternSyntaxException e) {
            throw new ParseException(e.getMessage(), e.getIndex());
        }
    }

    public String valueToString(Object value) throws ParseException {
        if (value == null) {
            return "";
        }
        return ((Pattern) value).pattern();
    }
}
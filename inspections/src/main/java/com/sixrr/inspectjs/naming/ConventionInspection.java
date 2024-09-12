package com.sixrr.inspectjs.naming;

import com.sixrr.inspectjs.JavaScriptInspection;

import java.util.regex.Matcher;

public abstract class ConventionInspection extends JavaScriptInspection {
    boolean isValid(String name, ConventionInspectionState state) {
        final int length = name.length();
        if (length < state.m_minLength) {
            return false;
        }
        if (length > state.m_maxLength) {
            return false;
        }
        if ("serialVersionUID".equals(name)) {
            return true;
        }
        final Matcher matcher = state.getPattern().matcher(name);
        return matcher.matches();
    }

    private static final int REGEX_COLUMN_COUNT = 25;

//	@Override
//	public JComponent createOptionsPanel()
//	{
//		final GridBagLayout layout = new GridBagLayout();
//		final JPanel panel = new JPanel(layout);
//
//		final JLabel patternLabel = new JLabel(InspectionJSBundle.message("pattern.parameter"));
//		patternLabel.setHorizontalAlignment(SwingConstants.TRAILING);
//		final JLabel minLengthLabel = new JLabel(InspectionJSBundle.message("min.length.parameter"));
//		minLengthLabel.setHorizontalAlignment(SwingConstants.TRAILING);
//		final JLabel maxLengthLabel = new JLabel(InspectionJSBundle.message("max.length.parameter"));
//		maxLengthLabel.setHorizontalAlignment(SwingConstants.TRAILING);
//
//		final NumberFormat numberFormat = NumberFormat.getIntegerInstance();
//		numberFormat.setParseIntegerOnly(true);
//		numberFormat.setMinimumIntegerDigits(1);
//		numberFormat.setMaximumIntegerDigits(2);
//		final InternationalFormatter formatter = new InternationalFormatter(numberFormat);
//		formatter.setAllowsInvalid(false);
//		formatter.setCommitsOnValidEdit(true);
//
//		final JFormattedTextField minLengthField = new JFormattedTextField(formatter);
//		final Font panelFont = panel.getFont();
//		minLengthField.setFont(panelFont);
//		minLengthField.setValue(m_minLength);
//		minLengthField.setColumns(2);
//		FormattedTextFieldMacFix.apply(minLengthField);
//
//		final JFormattedTextField maxLengthField = new JFormattedTextField(formatter);
//		maxLengthField.setFont(panelFont);
//		maxLengthField.setValue(m_maxLength);
//		maxLengthField.setColumns(2);
//		FormattedTextFieldMacFix.apply(maxLengthField);
//
//		final JFormattedTextField regexField = new JFormattedTextField(new RegExFormatter());
//		regexField.setFont(panelFont);
//		regexField.setValue(m_regexPattern);
//		regexField.setColumns(REGEX_COLUMN_COUNT);
//		regexField.setInputVerifier(new RegExInputVerifier());
//		regexField.setFocusLostBehavior(JFormattedTextField.COMMIT);
//		FormattedTextFieldMacFix.apply(regexField);
//		final DocumentListener listener = new DocumentListener()
//		{
//			@Override
//			public void changedUpdate(DocumentEvent e)
//			{
//				textChanged();
//			}
//
//			@Override
//			public void insertUpdate(DocumentEvent e)
//			{
//				textChanged();
//			}
//
//			@Override
//			public void removeUpdate(DocumentEvent e)
//			{
//				textChanged();
//			}
//
//			private void textChanged()
//			{
//				m_regexPattern = (Pattern) regexField.getValue();
//				m_regex = m_regexPattern.pattern();
//				m_minLength = ((Number) minLengthField.getValue()).intValue();
//				m_maxLength = ((Number) maxLengthField.getValue()).intValue();
//			}
//		};
//		final Document regexDocument = regexField.getDocument();
//		regexDocument.addDocumentListener(listener);
//		final Document minLengthDocument = minLengthField.getDocument();
//		minLengthDocument.addDocumentListener(listener);
//		final Document maxLengthDocument = maxLengthField.getDocument();
//		maxLengthDocument.addDocumentListener(listener);
//
//		final GridBagConstraints constraints = new GridBagConstraints();
//		constraints.gridx = 0;
//		constraints.gridy = 0;
//		constraints.weightx = 1.0;
//		constraints.anchor = GridBagConstraints.EAST;
//		constraints.fill = GridBagConstraints.HORIZONTAL;
//		panel.add(patternLabel, constraints);
//
//		constraints.gridx = 1;
//		constraints.gridy = 0;
//		constraints.gridwidth = 3;
//		constraints.anchor = GridBagConstraints.WEST;
//		panel.add(regexField, constraints);
//
//		constraints.gridx = 0;
//		constraints.gridy = 1;
//		constraints.gridwidth = 1;
//		constraints.anchor = GridBagConstraints.EAST;
//		panel.add(minLengthLabel, constraints);
//
//		constraints.gridx = 1;
//		constraints.gridy = 1;
//		constraints.anchor = GridBagConstraints.WEST;
//		panel.add(minLengthField, constraints);
//
//		constraints.gridx = 2;
//		constraints.gridy = 1;
//		constraints.anchor = GridBagConstraints.EAST;
//		panel.add(maxLengthLabel, constraints);
//
//		constraints.gridx = 3;
//		constraints.gridy = 1;
//		constraints.anchor = GridBagConstraints.WEST;
//		panel.add(maxLengthField, constraints);
//
//		return panel;
//	}
}

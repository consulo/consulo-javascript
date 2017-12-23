package com.sixrr.inspectjs.functionmetrics;

import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.ui.SingleIntegerFieldOptionsPanel;

import javax.swing.*;

public abstract class FunctionMetricsInspection extends JavaScriptInspection {
    @SuppressWarnings({"PublicField"})
    public int m_limit = getDefaultLimit();

    protected abstract int getDefaultLimit();

    protected abstract String getConfigurationLabel();

    protected int getLimit() {
        return m_limit;
    }

    @Override
	public JComponent createOptionsPanel() {
        final String configurationLabel = getConfigurationLabel();
        return new SingleIntegerFieldOptionsPanel(configurationLabel, this, "m_limit");
    }
}

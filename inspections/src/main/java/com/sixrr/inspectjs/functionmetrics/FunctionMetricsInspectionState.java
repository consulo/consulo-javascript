package com.sixrr.inspectjs.functionmetrics;

import consulo.configurable.ConfigurableBuilder;
import consulo.configurable.ConfigurableBuilderState;
import consulo.configurable.UnnamedConfigurable;
import consulo.localize.LocalizeValue;
import consulo.ui.IntBox;
import consulo.ui.Label;
import consulo.util.xml.serializer.XmlSerializerUtil;
import consulo.util.xml.serializer.annotation.Transient;

/**
 * @author VISTALL
 * @since 13/03/2023
 */
public class FunctionMetricsInspectionState<T extends FunctionMetricsInspectionState<T>> {
    @Transient
    private final LocalizeValue myLabelText;

    public int m_limit;

    public FunctionMetricsInspectionState(int m_limit, LocalizeValue labelText) {
        this.m_limit = m_limit;
        myLabelText = labelText;
    }

    public int getLimit() {
        return m_limit;
    }

    public UnnamedConfigurable createConfigurable() {
        ConfigurableBuilder<ConfigurableBuilderState> builder = ConfigurableBuilder.newBuilder();
        builder.component(() -> Label.create(myLabelText));
        builder.valueComponent(IntBox::create, () -> m_limit, it -> m_limit = it);
        return builder.buildUnnamed();
    }

    @SuppressWarnings("unchecked")
    public T getState() {
        return (T)this;
    }

    public void loadState(T state) {
        XmlSerializerUtil.copyBean(state, this);
    }
}

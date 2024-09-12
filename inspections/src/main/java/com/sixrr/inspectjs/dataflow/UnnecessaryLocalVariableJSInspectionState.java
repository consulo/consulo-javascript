package com.sixrr.inspectjs.dataflow;

import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.configurable.ConfigurableBuilder;
import consulo.configurable.ConfigurableBuilderState;
import consulo.configurable.UnnamedConfigurable;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.util.xml.serializer.XmlSerializerUtil;

import jakarta.annotation.Nullable;

/**
 * @author VISTALL
 * @since 13/03/2023
 */
public class UnnecessaryLocalVariableJSInspectionState implements InspectionToolState<UnnecessaryLocalVariableJSInspectionState> {
    public boolean m_ignoreImmediatelyReturnedVariables = false;

    @Nullable
    @Override
    public UnnamedConfigurable createConfigurable() {
        ConfigurableBuilder<ConfigurableBuilderState> builder = ConfigurableBuilder.newBuilder();
        builder.checkBox(
            InspectionJSLocalize.redundantLocalVariableIgnoreOption(),
            () -> m_ignoreImmediatelyReturnedVariables,
            b -> m_ignoreImmediatelyReturnedVariables = b
        );
        return builder.buildUnnamed();
    }

    @Nullable
    @Override
    public UnnecessaryLocalVariableJSInspectionState getState() {
        return this;
    }

    @Override
    public void loadState(UnnecessaryLocalVariableJSInspectionState state) {
        XmlSerializerUtil.copyBean(state, this);
    }
}

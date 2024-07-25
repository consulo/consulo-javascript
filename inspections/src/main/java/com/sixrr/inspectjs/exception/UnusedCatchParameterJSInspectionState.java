package com.sixrr.inspectjs.exception;

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
public class UnusedCatchParameterJSInspectionState implements InspectionToolState<UnusedCatchParameterJSInspectionState> {
    public boolean m_ignoreCatchBlocksWithComments = false;

    @Nullable
    @Override
    public UnnamedConfigurable createConfigurable() {
        ConfigurableBuilder<ConfigurableBuilderState> builder = ConfigurableBuilder.newBuilder();
        builder.checkBox(
            InspectionJSLocalize.unusedCatchParameterIgnoreCatchOption(),
            () -> m_ignoreCatchBlocksWithComments,
            b -> m_ignoreCatchBlocksWithComments = b
        );
        return builder.buildUnnamed();
    }

    @Nullable
    @Override
    public UnusedCatchParameterJSInspectionState getState() {
        return this;
    }

    @Override
    public void loadState(UnusedCatchParameterJSInspectionState state) {
        XmlSerializerUtil.copyBean(state, this);
    }
}

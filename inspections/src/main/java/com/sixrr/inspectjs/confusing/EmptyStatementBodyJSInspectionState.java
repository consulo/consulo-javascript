package com.sixrr.inspectjs.confusing;

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
public class EmptyStatementBodyJSInspectionState implements InspectionToolState<EmptyStatementBodyJSInspectionState> {
    public boolean m_reportEmptyBlocks = false;

    @Nullable
    @Override
    public UnnamedConfigurable createConfigurable() {
        ConfigurableBuilder<ConfigurableBuilderState> builder = ConfigurableBuilder.newBuilder();
        builder.checkBox(
            InspectionJSLocalize.includeStatementBodiesThatAreEmptyCodeBlocksParameter(),
            () -> m_reportEmptyBlocks,
            b -> m_reportEmptyBlocks = b
        );
        return builder.buildUnnamed();
    }

    @Nullable
    @Override
    public EmptyStatementBodyJSInspectionState getState() {
        return this;
    }

    @Override
    public void loadState(EmptyStatementBodyJSInspectionState state) {
        XmlSerializerUtil.copyBean(state, this);
    }
}

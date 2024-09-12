package com.sixrr.inspectjs.bitwise;

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
public class PointlessBitwiseExpressionJSInspectionState implements InspectionToolState<PointlessBitwiseExpressionJSInspectionState> {
    public boolean m_ignoreExpressionsContainingConstants = false;

    @Nullable
    @Override
    public UnnamedConfigurable createConfigurable() {
        ConfigurableBuilder<ConfigurableBuilderState> builder = ConfigurableBuilder.newBuilder();
        builder.checkBox(
            InspectionJSLocalize.pointlessBitwiseExpressionIgnoreOption(),
            () -> m_ignoreExpressionsContainingConstants,
            b -> m_ignoreExpressionsContainingConstants = b
        );
        return builder.buildUnnamed();
    }

    @Nullable
    @Override
    public PointlessBitwiseExpressionJSInspectionState getState() {
        return this;
    }

    @Override
    public void loadState(PointlessBitwiseExpressionJSInspectionState state) {
        XmlSerializerUtil.copyBean(state, this);
    }
}

package com.sixrr.inspectjs.confusing;

import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.configurable.ConfigurableBuilder;
import consulo.configurable.ConfigurableBuilderState;
import consulo.configurable.UnnamedConfigurable;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.ui.Label;
import consulo.util.xml.serializer.XmlSerializerUtil;

import jakarta.annotation.Nullable;

/**
 * @author VISTALL
 * @since 2023-03-18
 */
public class OverlyComplexArithmeticExpressionJSInspectionState implements InspectionToolState<OverlyComplexArithmeticExpressionJSInspectionState> {
    public int myMLimit = 6;

    @Nullable
    @Override
    public UnnamedConfigurable createConfigurable() {
        return ConfigurableBuilder.newBuilder()
            .component(() -> Label.create(InspectionJSLocalize.maximumNumberOfTermsParameter()))
            .intBox(() -> myMLimit, value -> myMLimit = value)
            .buildUnnamed();
    }

    @Nullable
    @Override
    public OverlyComplexArithmeticExpressionJSInspectionState getState() {
        return this;
    }

    @Override
    public void loadState(OverlyComplexArithmeticExpressionJSInspectionState state) {
        XmlSerializerUtil.copyBean(state, this);
    }
}

package com.sixrr.inspectjs.confusing;

import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.configurable.ConfigurableBuilder;
import consulo.configurable.ConfigurableBuilderState;
import consulo.configurable.UnnamedConfigurable;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.ui.Label;
import consulo.util.xml.serializer.XmlSerializerUtil;

import javax.annotation.Nullable;

/**
 * @author VISTALL
 * @since 18/03/2023
 */
public class OverlyComplexBooleanExpressionJSInspectionState implements InspectionToolState<OverlyComplexBooleanExpressionJSInspectionState>
{
	public int m_limit = 3;

	@Nullable
	@Override
	public UnnamedConfigurable createConfigurable()
	{
		ConfigurableBuilder<ConfigurableBuilderState> builder = ConfigurableBuilder.newBuilder();
		builder.component(() -> Label.create(InspectionJSLocalize.maximumNumberOfTermsParameter()));
		builder.intBox(() -> m_limit, value -> m_limit = value);
		return builder.buildUnnamed();
	}

	@Nullable
	@Override
	public OverlyComplexBooleanExpressionJSInspectionState getState()
	{
		return this;
	}

	@Override
	public void loadState(OverlyComplexBooleanExpressionJSInspectionState state)
	{
		XmlSerializerUtil.copyBean(state, this);
	}
}

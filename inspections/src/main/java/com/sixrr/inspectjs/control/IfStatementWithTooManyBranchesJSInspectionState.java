package com.sixrr.inspectjs.control;

import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.configurable.ConfigurableBuilder;
import consulo.configurable.ConfigurableBuilderState;
import consulo.configurable.UnnamedConfigurable;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.ui.IntBox;
import consulo.ui.Label;
import consulo.util.xml.serializer.XmlSerializerUtil;

import javax.annotation.Nullable;

/**
 * @author VISTALL
 * @since 13/03/2023
 */
public class IfStatementWithTooManyBranchesJSInspectionState implements InspectionToolState<IfStatementWithTooManyBranchesJSInspectionState>
{
	public int m_limit = 3;

	@Nullable
	@Override
	public UnnamedConfigurable createConfigurable()
	{
		ConfigurableBuilder<ConfigurableBuilderState> builder = ConfigurableBuilder.newBuilder();
		builder.component(() -> Label.create(InspectionJSLocalize.maximumNumberOfBranchesParameter()));
		builder.valueComponent(IntBox::create, () -> m_limit, it -> m_limit = it);
		return builder.buildUnnamed();
	}

	@Nullable
	@Override
	public IfStatementWithTooManyBranchesJSInspectionState getState()
	{
		return this;
	}

	@Override
	public void loadState(IfStatementWithTooManyBranchesJSInspectionState state)
	{
		XmlSerializerUtil.copyBean(state, this);
	}
}

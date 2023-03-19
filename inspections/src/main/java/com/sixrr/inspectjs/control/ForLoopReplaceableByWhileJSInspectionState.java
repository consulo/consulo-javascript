package com.sixrr.inspectjs.control;

import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.configurable.ConfigurableBuilder;
import consulo.configurable.ConfigurableBuilderState;
import consulo.configurable.UnnamedConfigurable;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.util.xml.serializer.XmlSerializerUtil;

import javax.annotation.Nullable;

/**
 * @author VISTALL
 * @since 13/03/2023
 */
public class ForLoopReplaceableByWhileJSInspectionState implements InspectionToolState<ForLoopReplaceableByWhileJSInspectionState>
{
	public boolean m_ignoreLoopsWithoutConditions = false;

	@Nullable
	@Override
	public UnnamedConfigurable createConfigurable()
	{
		ConfigurableBuilder<ConfigurableBuilderState> builder = ConfigurableBuilder.newBuilder();
		builder.checkBox(InspectionJSLocalize.forLoopReplaceableByWhileIgnoreOption(), () -> m_ignoreLoopsWithoutConditions, b -> m_ignoreLoopsWithoutConditions = b);
		return builder.buildUnnamed();
	}

	@Nullable
	@Override
	public ForLoopReplaceableByWhileJSInspectionState getState()
	{
		return this;
	}

	@Override
	public void loadState(ForLoopReplaceableByWhileJSInspectionState state)
	{
		XmlSerializerUtil.copyBean(state, this);
	}
}

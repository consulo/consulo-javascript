package com.sixrr.inspectjs.confusing;

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
public class NestedFunctionJSInspectionState implements InspectionToolState<NestedFunctionJSInspectionState>
{
	public boolean m_includeAnonymousFunctions = false;

	@Nullable
	@Override
	public UnnamedConfigurable createConfigurable()
	{
		ConfigurableBuilder<ConfigurableBuilderState> builder = ConfigurableBuilder.newBuilder();
		builder.checkBox(InspectionJSLocalize.includeAnonymousFunctionsParameter(), () -> m_includeAnonymousFunctions, b -> m_includeAnonymousFunctions = b);
		return builder.buildUnnamed();
	}

	@Nullable
	@Override
	public NestedFunctionJSInspectionState getState()
	{
		return this;
	}

	@Override
	public void loadState(NestedFunctionJSInspectionState state)
	{
		XmlSerializerUtil.copyBean(state, this);
	}
}

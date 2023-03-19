package com.sixrr.inspectjs.naming;

import consulo.configurable.UnnamedConfigurable;
import consulo.language.editor.inspection.InspectionToolState;

import javax.annotation.Nullable;

/**
 * @author VISTALL
 * @since 18/03/2023
 */
public class FunctionNamingConventionJSInspectionState extends ConventionInspectionState<FunctionNamingConventionJSInspectionState> implements InspectionToolState<FunctionNamingConventionJSInspectionState>
{
	public FunctionNamingConventionJSInspectionState()
	{
		super("[a-z][A-Za-z]*", 4, 32);
	}
}

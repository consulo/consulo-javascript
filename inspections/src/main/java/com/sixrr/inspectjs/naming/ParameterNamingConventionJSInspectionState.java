package com.sixrr.inspectjs.naming;

import consulo.language.editor.inspection.InspectionToolState;

/**
 * @author VISTALL
 * @since 18/03/2023
 */
public class ParameterNamingConventionJSInspectionState extends ConventionInspectionState<ParameterNamingConventionJSInspectionState> implements InspectionToolState<ParameterNamingConventionJSInspectionState>
{
	public ParameterNamingConventionJSInspectionState()
	{
		super("[a-z][A-Za-z]*", 1, 32);
	}
}

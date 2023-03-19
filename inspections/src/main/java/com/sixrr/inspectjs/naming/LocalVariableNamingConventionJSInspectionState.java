package com.sixrr.inspectjs.naming;

import consulo.language.editor.inspection.InspectionToolState;

/**
 * @author VISTALL
 * @since 18/03/2023
 */
public class LocalVariableNamingConventionJSInspectionState extends ConventionInspectionState<LocalVariableNamingConventionJSInspectionState> implements InspectionToolState<LocalVariableNamingConventionJSInspectionState>
{
	public LocalVariableNamingConventionJSInspectionState()
	{
		super("[a-z][A-Za-z]*", 1, 32);
	}
}

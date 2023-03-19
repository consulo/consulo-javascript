package com.sixrr.inspectjs.functionmetrics;

import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.language.editor.inspection.InspectionToolState;

/**
 * @author VISTALL
 * @since 13/03/2023
 */
public class CyclomaticComplexityJSInspectionState extends FunctionMetricsInspectionState<CyclomaticComplexityJSInspectionState> implements InspectionToolState<CyclomaticComplexityJSInspectionState>
{
	public CyclomaticComplexityJSInspectionState()
	{
		super(10, InspectionJSLocalize.overlyComplexFunctionDisplayName());
	}
}

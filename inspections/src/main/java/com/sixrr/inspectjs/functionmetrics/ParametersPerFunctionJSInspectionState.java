package com.sixrr.inspectjs.functionmetrics;

import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.language.editor.inspection.InspectionToolState;

/**
 * @author VISTALL
 * @since 13/03/2023
 */
public class ParametersPerFunctionJSInspectionState extends FunctionMetricsInspectionState<ParametersPerFunctionJSInspectionState> implements InspectionToolState<ParametersPerFunctionJSInspectionState> {
    public ParametersPerFunctionJSInspectionState() {
        super(5, InspectionJSLocalize.functionParameterLimit());
    }
}

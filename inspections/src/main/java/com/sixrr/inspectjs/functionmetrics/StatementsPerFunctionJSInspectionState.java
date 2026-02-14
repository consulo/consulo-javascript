package com.sixrr.inspectjs.functionmetrics;

import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.localize.LocalizeValue;

/**
 * @author VISTALL
 * @since 13/03/2023
 */
public class StatementsPerFunctionJSInspectionState extends FunctionMetricsInspectionState<StatementsPerFunctionJSInspectionState> implements InspectionToolState<StatementsPerFunctionJSInspectionState> {
    public StatementsPerFunctionJSInspectionState() {
        super(30, InspectionJSLocalize.maximumStatementsPerFunction());
    }
}

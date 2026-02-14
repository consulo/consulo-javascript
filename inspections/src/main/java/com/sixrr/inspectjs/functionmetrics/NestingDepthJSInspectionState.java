package com.sixrr.inspectjs.functionmetrics;

import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.language.editor.inspection.InspectionToolState;

/**
 * @author VISTALL
 * @since 13/03/2023
 */
public class NestingDepthJSInspectionState extends FunctionMetricsInspectionState<NestingDepthJSInspectionState> implements InspectionToolState<NestingDepthJSInspectionState> {
    public NestingDepthJSInspectionState() {
        super(5, InspectionJSLocalize.nestingDepthLimit());
    }
}

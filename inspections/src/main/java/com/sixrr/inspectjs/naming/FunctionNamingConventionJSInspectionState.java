package com.sixrr.inspectjs.naming;

import consulo.language.editor.inspection.InspectionToolState;

/**
 * @author VISTALL
 * @since 18/03/2023
 */
public class FunctionNamingConventionJSInspectionState extends ConventionInspectionState<FunctionNamingConventionJSInspectionState>
    implements InspectionToolState<FunctionNamingConventionJSInspectionState> {
    public FunctionNamingConventionJSInspectionState() {
        super("[a-z][A-Za-z]*", 4, 32);
    }
}

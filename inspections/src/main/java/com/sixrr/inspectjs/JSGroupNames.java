package com.sixrr.inspectjs;

import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.localize.LocalizeValue;

public interface JSGroupNames {
    LocalizeValue ERRORHANDLING_GROUP_NAME = InspectionJSLocalize.errorHandlingJavascriptGroupName();
    LocalizeValue CONTROL_FLOW_GROUP_NAME = InspectionJSLocalize.controlFlowIssuesJavascriptGroupName();
    LocalizeValue VALIDITY_GROUP_NAME = InspectionJSLocalize.javascriptValidityIssuesGroupName();
    LocalizeValue CONFUSING_GROUP_NAME = InspectionJSLocalize.potentiallyConfusingCodeConstructsJavascriptGroupName();
    LocalizeValue FUNCTIONMETRICS_GROUP_NAME = InspectionJSLocalize.javascriptFunctionMetricsGroupName();
    LocalizeValue ASSIGNMENT_GROUP_NAME = InspectionJSLocalize.assignmentIssuesJavascriptGroupName();
    LocalizeValue NAMING_CONVENTIONS_GROUP_NAME = InspectionJSLocalize.namingConventionsJavascriptGroupName();
    LocalizeValue STYLE_GROUP_NAME = InspectionJSLocalize.codeStyleIssuesJavascriptGroupName();
    LocalizeValue DOM_GROUP_NAME = InspectionJSLocalize.javascriptDomIssuesGroupName();
    LocalizeValue BUGS_GROUP_NAME = InspectionJSLocalize.probableBugsJavascriptGroupName();
    LocalizeValue MATURITY_GROUP_NAME = InspectionJSLocalize.codeMaturityJavascriptGroupName();
    LocalizeValue DATA_FLOW_ISSUES = InspectionJSLocalize.dataFlowIssuesJavascriptGroupName();
    LocalizeValue BITWISE_GROUP_NAME = InspectionJSLocalize.bitwiseIssuesJavascriptGroupName();
}

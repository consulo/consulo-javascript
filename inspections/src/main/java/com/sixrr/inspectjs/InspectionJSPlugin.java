package com.sixrr.inspectjs;

import java.util.ArrayList;
import java.util.List;

import com.intellij.codeInspection.InspectionToolProvider;
import com.intellij.codeInspection.LocalInspectionTool;
import com.sixrr.inspectjs.assignment.AssignmentResultUsedJSInspection;
import com.sixrr.inspectjs.assignment.AssignmentToForLoopParameterJSInspection;
import com.sixrr.inspectjs.assignment.AssignmentToFunctionParameterJSInspection;
import com.sixrr.inspectjs.assignment.NestedAssignmentJSInspection;
import com.sixrr.inspectjs.assignment.ReplaceAssignmentWithOperatorAssignmentJSInspection;
import com.sixrr.inspectjs.assignment.SillyAssignmentJSInspection;
import com.sixrr.inspectjs.bitwise.IncompatibleMaskJSInspection;
import com.sixrr.inspectjs.bitwise.PointlessBitwiseExpressionJSInspection;
import com.sixrr.inspectjs.bitwise.ShiftOutOfRangeJSInspection;
import com.sixrr.inspectjs.bugs.DivideByZeroJSInspection;
import com.sixrr.inspectjs.bugs.EqualityComparisonWithCoercionJSInspection;
import com.sixrr.inspectjs.bugs.InfiniteLoopJSInspection;
import com.sixrr.inspectjs.bugs.InfiniteRecursionJSInspection;
import com.sixrr.inspectjs.bugs.NonShortCircuitBooleanExpressionJSInspection;
import com.sixrr.inspectjs.bugs.ObjectAllocationIgnoredJSInspection;
import com.sixrr.inspectjs.bugs.TextLabelInSwitchStatementJSInspection;
import com.sixrr.inspectjs.confusing.*;
import com.sixrr.inspectjs.control.*;
import com.sixrr.inspectjs.dataflow.ReuseOfLocalVariableJSInspection;
import com.sixrr.inspectjs.dataflow.UnnecessaryLocalVariableJSInspection;
import com.sixrr.inspectjs.dom.DocumentWriteJSInspection;
import com.sixrr.inspectjs.dom.InnerHTMLJSInspection;
import com.sixrr.inspectjs.dom.PlatformDetectionJSInspection;
import com.sixrr.inspectjs.dom.XHTMLIncompatabilitiesJSInspection;
import com.sixrr.inspectjs.exception.ContinueOrBreakFromFinallyBlockJSInspection;
import com.sixrr.inspectjs.exception.EmptyCatchBlockJSInspection;
import com.sixrr.inspectjs.exception.EmptyFinallyBlockJSInspection;
import com.sixrr.inspectjs.exception.EmptyTryBlockJSInspection;
import com.sixrr.inspectjs.exception.ExceptionCaughtLocallyJSInspection;
import com.sixrr.inspectjs.exception.ReturnFromFinallyBlockJSInspection;
import com.sixrr.inspectjs.exception.ThrowFromFinallyBlockJSInspection;
import com.sixrr.inspectjs.exception.UnusedCatchParameterJSInspection;
import com.sixrr.inspectjs.functionmetrics.CyclomaticComplexityJSInspection;
import com.sixrr.inspectjs.functionmetrics.FunctionWithMultipleLoopsJSInspection;
import com.sixrr.inspectjs.functionmetrics.FunctionWithMultipleReturnPointsJSInspection;
import com.sixrr.inspectjs.functionmetrics.NestingDepthJSInspection;
import com.sixrr.inspectjs.functionmetrics.ParametersPerFunctionJSInspection;
import com.sixrr.inspectjs.functionmetrics.StatementsPerFunctionJSInspection;
import com.sixrr.inspectjs.functionmetrics.ThreeNegationsPerFunctionJSInspection;
import com.sixrr.inspectjs.naming.FunctionNamingConventionJSInspection;
import com.sixrr.inspectjs.naming.LocalVariableNamingConventionJSInspection;
import com.sixrr.inspectjs.naming.ParameterNamingConventionJSInspection;
import com.sixrr.inspectjs.style.ChainedEqualityJSInspection;
import com.sixrr.inspectjs.style.ChainedFunctionCallJSInspection;
import com.sixrr.inspectjs.style.ConstantOnLHSOfComparisonJSInspection;
import com.sixrr.inspectjs.style.ConstantOnRHSOfComparisonJSInspection;
import com.sixrr.inspectjs.style.NestedFunctionCallJSInspection;
import com.sixrr.inspectjs.style.NonBlockStatementBodyJSInspection;
import com.sixrr.inspectjs.style.UnterminatedStatementJSInspection;
import com.sixrr.inspectjs.validity.BadExpressionStatementJSInspection;
import com.sixrr.inspectjs.validity.DebuggerStatementJSInspection;
import com.sixrr.inspectjs.validity.DuplicateCaseLabelJSInspection;
import com.sixrr.inspectjs.validity.DuplicatePropertyOnObjectJSInspection;
import com.sixrr.inspectjs.validity.FunctionWithInconsistentReturnsJSInspection;
import com.sixrr.inspectjs.validity.StringLiteralBreaksHTMLJSInspection;
import com.sixrr.inspectjs.validity.ThisExpressionReferencesGlobalObjectJSInspection;
import com.sixrr.inspectjs.validity.UnreachableCodeJSInspection;

public class InspectionJSPlugin implements InspectionToolProvider
{
	private static final int NUM_INSPECTIONS = 100;
	private final List<Class<? extends LocalInspectionTool>> m_inspectionClasses = new ArrayList<Class<? extends LocalInspectionTool>>(NUM_INSPECTIONS);

	@Override
	public Class<? extends LocalInspectionTool>[] getInspectionClasses()
	{
		if(m_inspectionClasses.isEmpty())
		{
			registerDOMInspections();
			registerControlFlowInspections();
			registerAssignmentInspections();
			registerBugInspections();
			registerValidityInspections();
			registerNamingInspections();
			registerConfusingInspections();
			registerExceptionInspections();
			registerStyleInspections();
			registerFunctionMetricsInspections();
			registerDataflowInspections();
			registerBitwiseInspections();
		}
		final int numInspections = m_inspectionClasses.size();
		return m_inspectionClasses.toArray(new Class[numInspections]);
	}

	private void registerAssignmentInspections()
	{
		m_inspectionClasses.add(AssignmentResultUsedJSInspection.class);
		m_inspectionClasses.add(NestedAssignmentJSInspection.class);
		m_inspectionClasses.add(AssignmentToFunctionParameterJSInspection.class);
		m_inspectionClasses.add(AssignmentToForLoopParameterJSInspection.class);
		m_inspectionClasses.add(ReplaceAssignmentWithOperatorAssignmentJSInspection.class);
		m_inspectionClasses.add(SillyAssignmentJSInspection.class);
	}

	private void registerBugInspections()
	{
		m_inspectionClasses.add(TextLabelInSwitchStatementJSInspection.class);
		m_inspectionClasses.add(NonShortCircuitBooleanExpressionJSInspection.class);
		m_inspectionClasses.add(ObjectAllocationIgnoredJSInspection.class);
		m_inspectionClasses.add(DivideByZeroJSInspection.class);
		m_inspectionClasses.add(EqualityComparisonWithCoercionJSInspection.class);
	}

	private void registerBitwiseInspections()
	{
		m_inspectionClasses.add(PointlessBitwiseExpressionJSInspection.class);
		m_inspectionClasses.add(ShiftOutOfRangeJSInspection.class);
		m_inspectionClasses.add(IncompatibleMaskJSInspection.class);
	}

	private void registerControlFlowInspections()
	{
		m_inspectionClasses.add(LoopStatementThatDoesntLoopJSInspection.class);
		m_inspectionClasses.add(InfiniteLoopJSInspection.class);
		m_inspectionClasses.add(InfiniteRecursionJSInspection.class);
		m_inspectionClasses.add(PointlessBooleanExpressionJSInspection.class);
		m_inspectionClasses.add(PointlessArithmeticExpressionJSInspection.class);
		m_inspectionClasses.add(NegatedIfStatementJSInspection.class);
		m_inspectionClasses.add(NegatedConditionalExpressionJSInspection.class);
		m_inspectionClasses.add(BreakStatementJSInspection.class);
		m_inspectionClasses.add(BreakStatementWithLabelJSInspection.class);
		m_inspectionClasses.add(ContinueStatementJSInspection.class);
		m_inspectionClasses.add(ContinueStatementWithLabelJSInspection.class);
		m_inspectionClasses.add(DefaultNotLastCaseInSwitchJSInspection.class);
		m_inspectionClasses.add(UnnecessaryContinueJSInspection.class);
		m_inspectionClasses.add(UnnecessaryReturnJSInspection.class);
		m_inspectionClasses.add(UnnecessaryLabelOnBreakStatementJSInspection.class);
		m_inspectionClasses.add(UnnecessaryLabelOnContinueStatementJSInspection.class);
		m_inspectionClasses.add(LabeledStatementJSInspection.class);
		m_inspectionClasses.add(SwitchStatementWithNoDefaultBranchJSInspection.class);
		m_inspectionClasses.add(FallthroughInSwitchStatementJSInspection.class);
		m_inspectionClasses.add(NestedSwitchStatementJSInspection.class);
		m_inspectionClasses.add(DuplicateConditionJSInspection.class);
		m_inspectionClasses.add(ConstantConditionalExpressionJSInspection.class);
		m_inspectionClasses.add(ConstantIfStatementJSInspection.class);
		m_inspectionClasses.add(ConditionalExpressionWithIdenticalBranchesJSInspection.class);
		m_inspectionClasses.add(IfStatementWithIdenticalBranchesJSInspection.class);
		m_inspectionClasses.add(IfStatementWithTooManyBranchesJSInspection.class);
		m_inspectionClasses.add(TrivialIfJSInspection.class);
		m_inspectionClasses.add(TrivialConditionalJSInspection.class);
		m_inspectionClasses.add(UnnecessaryLabelJSInspection.class);
		m_inspectionClasses.add(ForLoopThatDoesntUseLoopVariableJSInspection.class);
		m_inspectionClasses.add(TailRecursionJSInspection.class);
		m_inspectionClasses.add(ForLoopReplaceableByWhileJSInspection.class);
	}

	private void registerValidityInspections()
	{
		m_inspectionClasses.add(DebuggerStatementJSInspection.class);
		m_inspectionClasses.add(BadExpressionStatementJSInspection.class);
		m_inspectionClasses.add(DuplicateCaseLabelJSInspection.class);
		m_inspectionClasses.add(DuplicatePropertyOnObjectJSInspection.class);
		m_inspectionClasses.add(UnreachableCodeJSInspection.class);
		m_inspectionClasses.add(FunctionWithInconsistentReturnsJSInspection.class);
		m_inspectionClasses.add(ThisExpressionReferencesGlobalObjectJSInspection.class);
		m_inspectionClasses.add(StringLiteralBreaksHTMLJSInspection.class);
	}

	private void registerNamingInspections()
	{
		m_inspectionClasses.add(FunctionNamingConventionJSInspection.class);
		m_inspectionClasses.add(LocalVariableNamingConventionJSInspection.class);
		m_inspectionClasses.add(ParameterNamingConventionJSInspection.class);
	}

	private void registerConfusingInspections()
	{
		m_inspectionClasses.add(CallerJSInspection.class);
		m_inspectionClasses.add(BlockStatementJSInspection.class);
		m_inspectionClasses.add(ConditionalExpressionJSInspection.class);
		m_inspectionClasses.add(CommaExpressionJSInspection.class);
		m_inspectionClasses.add(NestedConditionalExpressionJSInspection.class);
		m_inspectionClasses.add(IncrementDecrementResultUsedJSInspection.class);
		m_inspectionClasses.add(WithStatementJSInspection.class);
		m_inspectionClasses.add(VoidExpressionJSInspection.class);
		m_inspectionClasses.add(OverlyComplexArithmeticExpressionJSInspection.class);
		m_inspectionClasses.add(OverlyComplexBooleanExpressionJSInspection.class);
		m_inspectionClasses.add(NestedFunctionJSInspection.class);
		m_inspectionClasses.add(AnonymousFunctionJSInspection.class);
		m_inspectionClasses.add(EmptyStatementBodyJSInspection.class);
		m_inspectionClasses.add(OctalIntegerJSInspection.class);
		m_inspectionClasses.add(ConfusingFloatingPointLiteralJSInspection.class);
		m_inspectionClasses.add(ConfusingPlusesOrMinusesJSInspection.class);
		m_inspectionClasses.add(DynamicallyGeneratedCodeJSInspection.class);
		m_inspectionClasses.add(MagicNumberJSInspection.class);
	}

	private void registerStyleInspections()
	{
		m_inspectionClasses.add(ConstantOnLHSOfComparisonJSInspection.class);
		m_inspectionClasses.add(ChainedEqualityJSInspection.class);
		m_inspectionClasses.add(ChainedFunctionCallJSInspection.class);
		m_inspectionClasses.add(NestedFunctionCallJSInspection.class);
		m_inspectionClasses.add(ConstantOnRHSOfComparisonJSInspection.class);
		m_inspectionClasses.add(UnterminatedStatementJSInspection.class);
		m_inspectionClasses.add(NonBlockStatementBodyJSInspection.class);
	}

	private void registerExceptionInspections()
	{
		m_inspectionClasses.add(EmptyCatchBlockJSInspection.class);
		m_inspectionClasses.add(EmptyTryBlockJSInspection.class);
		m_inspectionClasses.add(EmptyFinallyBlockJSInspection.class);
		m_inspectionClasses.add(ReturnFromFinallyBlockJSInspection.class);
		m_inspectionClasses.add(ThrowFromFinallyBlockJSInspection.class);
		m_inspectionClasses.add(ContinueOrBreakFromFinallyBlockJSInspection.class);
		m_inspectionClasses.add(ExceptionCaughtLocallyJSInspection.class);
		m_inspectionClasses.add(UnusedCatchParameterJSInspection.class);
	}

	private void registerFunctionMetricsInspections()
	{
		m_inspectionClasses.add(NestingDepthJSInspection.class);
		m_inspectionClasses.add(ThreeNegationsPerFunctionJSInspection.class);
		m_inspectionClasses.add(FunctionWithMultipleLoopsJSInspection.class);
		m_inspectionClasses.add(FunctionWithMultipleReturnPointsJSInspection.class);
		m_inspectionClasses.add(CyclomaticComplexityJSInspection.class);
		m_inspectionClasses.add(ParametersPerFunctionJSInspection.class);
		m_inspectionClasses.add(StatementsPerFunctionJSInspection.class);
	}

	private void registerDOMInspections()
	{
		m_inspectionClasses.add(DocumentWriteJSInspection.class);
		m_inspectionClasses.add(InnerHTMLJSInspection.class);
		m_inspectionClasses.add(PlatformDetectionJSInspection.class);
		m_inspectionClasses.add(XHTMLIncompatabilitiesJSInspection.class);
	}

	private void registerDataflowInspections()
	{
		m_inspectionClasses.add(UnnecessaryLocalVariableJSInspection.class);
		m_inspectionClasses.add(ReuseOfLocalVariableJSInspection.class);
	}
}

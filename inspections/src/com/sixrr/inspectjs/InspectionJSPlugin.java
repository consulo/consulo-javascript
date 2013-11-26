package com.sixrr.inspectjs;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.codeInspection.InspectionToolProvider;
import com.intellij.codeInspection.LocalInspectionTool;
import com.intellij.openapi.application.Application;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.ApplicationComponent;
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
import com.sixrr.inspectjs.validity.ReservedWordUsedAsNameJSInspection;
import com.sixrr.inspectjs.validity.StringLiteralBreaksHTMLJSInspection;
import com.sixrr.inspectjs.validity.ThisExpressionReferencesGlobalObjectJSInspection;
import com.sixrr.inspectjs.validity.UnreachableCodeJSInspection;

@SuppressWarnings({"OverlyCoupledClass",
        "OverlyCoupledMethod",
        "OverlyLongMethod",
        "ClassWithTooManyMethods"})
public class InspectionJSPlugin implements ApplicationComponent,
        InspectionToolProvider {

    private static final int NUM_INSPECTIONS = 100;
    private final List<Class<? extends LocalInspectionTool>> m_inspectionClasses =
            new ArrayList<Class<? extends LocalInspectionTool>>(NUM_INSPECTIONS);
    @NonNls
    private static final String DESCRIPTION_DIRECTORY_NAME =
            "src/inspectionDescriptions/";

    public static InspectionJSPlugin getInstance() {
        final Application application = ApplicationManager.getApplication();
        return application.getComponent(InspectionJSPlugin.class);
    }

    public static void main(String[] args) {
        final PrintStream out;
        if (args.length == 0) {
            out = System.out;
        } else {
            final OutputStream stream;
            try {
                stream = new FileOutputStream(args[0]);
            } catch (final FileNotFoundException e) {
                return;
            }
            out = new PrintStream(stream);
        }
        final InspectionJSPlugin plugin = new InspectionJSPlugin();
        plugin.createDocumentation(out);
       // plugin.createTestScaffolding(out);
    }

    private void createTestScaffolding(PrintStream out) {
        final Class<? extends LocalInspectionTool>[] classes = getInspectionClasses();
        for (Class<? extends LocalInspectionTool> aClass : classes) {
            final LocalInspectionTool inspection;
            try {
                inspection = aClass.newInstance();
            } catch (InstantiationException e) {
                e.printStackTrace();
                continue;
            } catch (IllegalAccessException e) {
                e.printStackTrace();
                continue;
            }
            final Package aPackage = aClass.getPackage();
            final String packageName = aPackage.getName();
            final String className = aClass.getName();
            final String shortName =
                    className.substring(packageName.length() + 1, className.length() - "JSInspection".length());
            out.println(shortName);
            out.println(packageName);
            final String testDataDir = "testdata" + File.separatorChar + shortName;
            final String testDataSrcDir = testDataDir + File.separatorChar + "src";
            new File(testDataSrcDir).mkdirs();
            makeResults(testDataDir, inspection.getDisplayName());
            makeTestJS(testDataSrcDir, shortName);
            final String testSrcDir = "testsrc" + File.separatorChar + packageName.replaceAll("\\.", File.separator);
            new File(testSrcDir).mkdirs();
            out.println(testSrcDir);
            makeTestCase(testSrcDir, packageName, shortName);
        }
    }

    private void makeTestCase(String testSrcDir, String packageName, String testName) {
        PrintWriter printWriter = null;
        try {
            printWriter = new PrintWriter(testSrcDir + File.separatorChar + testName + "Test.java");
            printWriter.println("package " + packageName +";\n" +
                    "\n" +
                    "import com.intellij.testFramework.*;\n" +
                    "import " + packageName + "."+ testName + "JSInspection;\n" +
                    "\n" +
                    "public class " + testName +"Test extends InspectionTestCase {\n" +
                    "\n" +
                    "  public void test() throws Exception {\n" +
                    "    doTest(\""+ testName +"/\", new "+ testName+ "JSInspection());\n" +
                    "  }\n" +
                    "}");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } finally {
            if (printWriter != null) {
                printWriter.close();
            }
        }
    }
    private void makeResults(String testDataDir, String testName) {
        PrintWriter printWriter = null;
        try {
            printWriter = new PrintWriter(testDataDir + File.separatorChar + "expected.xml");
            printWriter.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
                    "<problems>\n" +
                    "  <problem>\n" +
                    "    <file>test.js</file>\n" +
                    "    <line>2</line>\n" +
                    "    <problem_class>" + testName+ "</problem_class>\n" +
                    "    <description>invertion</description>\n" +
                    "  </problem>\n" +
                    "</problems>");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } finally {
            if (printWriter != null) {
                printWriter.close();
            }
        }
    }

    private void makeTestJS(String testDataSrcDir, String inspectionName) {
        PrintWriter printWriter = null;
        try {
            printWriter = new PrintWriter(testDataSrcDir + File.separatorChar + "test.js");
            printWriter.println("//test for inspection " + inspectionName);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } finally {
            if (printWriter != null) {
                printWriter.close();
            }
        }
    }

    @SuppressWarnings({"HardCodedStringLiteral"})
    @NonNls
    private void createDocumentation(PrintStream out) {
        final Class<? extends LocalInspectionTool>[] classes = getInspectionClasses();
        Arrays.sort(classes, new InspectionComparator());

        final int numQuickFixes = countQuickFixes(classes, out);
        out.println(classes.length + " Inspections");
        out.println(numQuickFixes + " Quick Fixes");
        String currentGroupName = "";

        for (final Class<? extends LocalInspectionTool> aClass : classes) {
            final String className = aClass.getName();
            try {
                final LocalInspectionTool inspection =
                        aClass.newInstance();
                final String groupDisplayName =
                        inspection.getGroupDisplayName();
                if (!groupDisplayName.equals(currentGroupName)) {
                    currentGroupName = groupDisplayName;
                    out.println();
                    out.print("   * ");
                    out.println(currentGroupName);
                }
                printInspectionDescription(inspection, out);
            } catch (InstantiationException ignore) {
                out.print("Couldn't instantiate ");
                out.println(className);
            } catch (IllegalAccessException ignore) {
                out.print("Couldn't access ");
                out.println(className);
            } catch (ClassCastException ignore) {
                out.print("Couldn't cast ");
                out.println(className);
            }
        }

        out.println();
        out.println("Inspections enabled by default:");
        for (final Class<? extends LocalInspectionTool> aClass : classes) {
            final String className = aClass.getName();
            try {
                final LocalInspectionTool inspection =
                        aClass.newInstance();
                if (inspection.isEnabledByDefault()) {
                    out.println('\t' + inspection.getDisplayName());
                }
            } catch (InstantiationException ignore) {
                out.print("Couldn't instantiate ");
                out.println(className);
            } catch (IllegalAccessException ignore) {
                out.print("Couldn't access ");
                out.println(className);
            } catch (ClassCastException ignore) {
                out.print("Couldn't cast ");
                out.println(className);
            }
        }
        final File descriptionDirectory = new File(DESCRIPTION_DIRECTORY_NAME);
        final File[] descriptionFiles = descriptionDirectory.listFiles();
        final Set<File> descriptionFilesSet = new HashSet<File>(descriptionFiles.length);
        for (File descriptionFile1 : descriptionFiles) {
            if (!descriptionFile1.getName().startsWith(".")) {
                descriptionFilesSet.add(descriptionFile1);
            }
        }
        for (final Class<? extends LocalInspectionTool> aClass : classes) {
            final String className = aClass.getName();
            final String simpleClassName =
                    className.substring(className.lastIndexOf('.') + 1,
                            className.length() -
                                    "Inspection".length());
            final String fileName =
                    DESCRIPTION_DIRECTORY_NAME + simpleClassName + ".html";
            final File descriptionFile = new File(fileName);
            if (descriptionFile.exists()) {
                descriptionFilesSet.remove(descriptionFile);
            } else {
                out.println("Couldn't find documentation file: " + fileName);
            }
        }
        for (final File file : descriptionFilesSet) {
            out.println("Unused documentation file: " + file.getAbsolutePath());
        }
    }

    @SuppressWarnings({"HardCodedStringLiteral"})
    private static void printInspectionDescription(LocalInspectionTool inspection,
                                                   PrintStream out) {
        final boolean hasQuickFix = ((BaseInspection) inspection).hasQuickFix();

        final String displayName = inspection.getDisplayName();
        out.print("      * ");
        out.print(displayName);
        if (hasQuickFix) {
            if (((BaseInspection) inspection).buildQuickFixesOnlyForOnTheFlyErrors()) {
                out.print("(r)");
            } else {
                out.print("(*)");
            }
        }
        out.println();
    }

    @SuppressWarnings({"HardCodedStringLiteral"})
    private static int countQuickFixes(Class<? extends LocalInspectionTool>[] classes, PrintStream out) {
        int numQuickFixes = 0;
        for (final Class<? extends LocalInspectionTool> aClass : classes) {
            final String className = aClass.getName();
            try {
                final LocalInspectionTool inspection =
                        aClass.newInstance();
                if (((BaseInspection) inspection).hasQuickFix()) {
                    numQuickFixes++;
                }
            } catch (InstantiationException ignore) {
                out.print("Couldn't instantiate ");
                out.println(className);
            } catch (IllegalAccessException ignore) {
                out.print("Couldn't access ");
                out.println(className);
            } catch (ClassCastException ignore) {
                out.print("Couldn't cast ");
                out.println(className);
            }
        }
        return numQuickFixes;
    }

    @NotNull
    public String getComponentName() {
        return "InspectionJS";
    }

    public Class<? extends LocalInspectionTool>[] getInspectionClasses() {
        if (m_inspectionClasses.isEmpty()) {
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

    public void initComponent() {
    }

    private void registerAssignmentInspections() {
        m_inspectionClasses.add(AssignmentResultUsedJSInspection.class);
        m_inspectionClasses.add(NestedAssignmentJSInspection.class);
        m_inspectionClasses.add(AssignmentToFunctionParameterJSInspection.class);
        m_inspectionClasses.add(AssignmentToForLoopParameterJSInspection.class);
        m_inspectionClasses.add(ReplaceAssignmentWithOperatorAssignmentJSInspection.class);
        m_inspectionClasses.add(SillyAssignmentJSInspection.class);
    }

    private void registerBugInspections() {
        m_inspectionClasses.add(TextLabelInSwitchStatementJSInspection.class);
        m_inspectionClasses.add(NonShortCircuitBooleanExpressionJSInspection.class);
        m_inspectionClasses.add(ObjectAllocationIgnoredJSInspection.class);
        m_inspectionClasses.add(DivideByZeroJSInspection.class);
        m_inspectionClasses.add(EqualityComparisonWithCoercionJSInspection.class);
    }

    private void registerBitwiseInspections() {
        m_inspectionClasses.add(PointlessBitwiseExpressionJSInspection.class);
        m_inspectionClasses.add(ShiftOutOfRangeJSInspection.class);
        m_inspectionClasses.add(IncompatibleMaskJSInspection.class);
    }

    private void registerControlFlowInspections() {
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

    private void registerValidityInspections() {
        m_inspectionClasses.add(DebuggerStatementJSInspection.class);
        m_inspectionClasses.add(BadExpressionStatementJSInspection.class);
        m_inspectionClasses.add(DuplicateCaseLabelJSInspection.class);
        m_inspectionClasses.add(DuplicatePropertyOnObjectJSInspection.class);
        m_inspectionClasses.add(UnreachableCodeJSInspection.class);
        m_inspectionClasses.add(FunctionWithInconsistentReturnsJSInspection.class);
        m_inspectionClasses.add(ThisExpressionReferencesGlobalObjectJSInspection.class);
        m_inspectionClasses.add(ReservedWordUsedAsNameJSInspection.class);
        m_inspectionClasses.add(StringLiteralBreaksHTMLJSInspection.class);
    }

    private void registerNamingInspections() {
        m_inspectionClasses.add(FunctionNamingConventionJSInspection.class);
        m_inspectionClasses.add(LocalVariableNamingConventionJSInspection.class);
        m_inspectionClasses.add(ParameterNamingConventionJSInspection.class);
    }

    private void registerConfusingInspections() {
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

    private void registerStyleInspections() {
        m_inspectionClasses.add(ConstantOnLHSOfComparisonJSInspection.class);
        m_inspectionClasses.add(ChainedEqualityJSInspection.class);
        m_inspectionClasses.add(ChainedFunctionCallJSInspection.class);
        m_inspectionClasses.add(NestedFunctionCallJSInspection.class);
        m_inspectionClasses.add(ConstantOnRHSOfComparisonJSInspection.class);
        m_inspectionClasses.add(UnterminatedStatementJSInspection.class);
        m_inspectionClasses.add(NonBlockStatementBodyJSInspection.class);
    }

    private void registerExceptionInspections() {
        m_inspectionClasses.add(EmptyCatchBlockJSInspection.class);
        m_inspectionClasses.add(EmptyTryBlockJSInspection.class);
        m_inspectionClasses.add(EmptyFinallyBlockJSInspection.class);
        m_inspectionClasses.add(ReturnFromFinallyBlockJSInspection.class);
        m_inspectionClasses.add(ThrowFromFinallyBlockJSInspection.class);
        m_inspectionClasses.add(ContinueOrBreakFromFinallyBlockJSInspection.class);
        m_inspectionClasses.add(ExceptionCaughtLocallyJSInspection.class);
        m_inspectionClasses.add(UnusedCatchParameterJSInspection.class);
    }

    private void registerFunctionMetricsInspections() {
        m_inspectionClasses.add(NestingDepthJSInspection.class);
        m_inspectionClasses.add(ThreeNegationsPerFunctionJSInspection.class);
        m_inspectionClasses.add(FunctionWithMultipleLoopsJSInspection.class);
        m_inspectionClasses.add(FunctionWithMultipleReturnPointsJSInspection.class);
        m_inspectionClasses.add(CyclomaticComplexityJSInspection.class);
        m_inspectionClasses.add(ParametersPerFunctionJSInspection.class);
        m_inspectionClasses.add(StatementsPerFunctionJSInspection.class);
    }

    private void registerDOMInspections() {
        m_inspectionClasses.add(DocumentWriteJSInspection.class);
        m_inspectionClasses.add(InnerHTMLJSInspection.class);
        m_inspectionClasses.add(PlatformDetectionJSInspection.class);
        m_inspectionClasses.add(XHTMLIncompatabilitiesJSInspection.class);
    }

    private void registerDataflowInspections() {
        m_inspectionClasses.add(UnnecessaryLocalVariableJSInspection.class);
        m_inspectionClasses.add(ReuseOfLocalVariableJSInspection.class);
    }

    public void disposeComponent() {
    }
}

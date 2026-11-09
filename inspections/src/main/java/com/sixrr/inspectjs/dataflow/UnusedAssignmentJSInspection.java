package com.sixrr.inspectjs.dataflow;

import com.intellij.lang.javascript.JSTokenTypes;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.impl.language.controlFlow.JavaScriptControlFlow;
import consulo.javascript.impl.language.controlFlow.JavaScriptFlowVariables;
import consulo.javascript.impl.language.controlFlow.JavaScriptUnusedValues;
import consulo.language.editor.inspection.ProblemHighlightType;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiUtilCore;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.localize.LocalizeValue;
import org.jspecify.annotations.Nullable;

/**
 * A value assigned to a local variable and never read: overwritten, or the variable is not read after it on any path -
 * the liveness of the control flow graph ({@link JavaScriptUnusedValues}). A variable never read at all is an unused
 * variable, not reported here.
 *
 * @author VISTALL
 */
@ExtensionImpl
public class UnusedAssignmentJSInspection extends JavaScriptInspection {
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.unusedAssignmentDisplayName();
    }

    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.DATA_FLOW_ISSUES;
    }

    @Override
    public boolean isEnabledByDefault() {
        return true;
    }

    @Nullable
    @Override
    @RequiredReadAction
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.unusedAssignmentErrorString(args[0]).get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        @RequiredReadAction
        public void visitElement(PsiElement element) {
            super.visitElement(element);
            if (!JavaScriptControlFlow.isOwner(element)) {
                return;
            }
            JavaScriptControlFlow flow = JavaScriptControlFlow.of(element);
            if (flow == null) {
                return;
            }
            for (JavaScriptUnusedValues.Write write : flow.getUnusedValues()) {
                // the name of a declaration, the reference assigned
                PsiElement location = write.element();
                if (location == write.variable()) {
                    for (PsiElement leaf = PsiTreeUtil.firstChild(location); leaf != null && PsiTreeUtil.isAncestor(location, leaf, false); leaf = PsiTreeUtil.nextLeaf(leaf)) {
                        if (PsiUtilCore.getElementType(leaf) == JSTokenTypes.IDENTIFIER) {
                            location = leaf;
                            break;
                        }
                    }
                }
                registerError(location, location, ProblemHighlightType.LIKE_UNUSED_SYMBOL, JavaScriptFlowVariables.name(write.variable()));
            }
        }
    }
}

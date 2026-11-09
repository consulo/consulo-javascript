package com.sixrr.inspectjs.validity;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.application.util.CachedValueProvider;
import consulo.javascript.impl.language.controlFlow.JavaScriptControlFlow;
import consulo.javascript.impl.language.controlFlow.JavaScriptFlowVariables;
import consulo.language.editor.rawHighlight.HighlightDisplayLevel;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiModificationTracker;
import consulo.language.psi.PsiNameIdentifierOwner;
import consulo.language.psi.PsiUtilCore;
import consulo.language.psi.util.LanguageCachedValueUtil;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.localize.LocalizeValue;
import org.jspecify.annotations.Nullable;

import java.util.HashSet;
import java.util.Set;

/**
 * A let, a const, a class or an enum used before its declaration - the temporal dead zone, checked as tsc does
 * ({@code isBlockScopedNameDeclaredBeforeUse}): by the position of the use, before the declaration or in its own
 * initializer, in the same function - not in a nested function or an instance property initializer, which run later;
 * an immediately invoked function and a class static block run at once (TS2448, TS2449, TS2450)
 *
 * @author VISTALL
 */
@ExtensionImpl
public class UsedBeforeDeclarationJSInspection extends JavaScriptInspection {
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.usedBeforeDeclarationDisplayName();
    }

    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.VALIDITY_GROUP_NAME;
    }

    @Override
    public boolean isEnabledByDefault() {
        return true;
    }

    @Override
    public HighlightDisplayLevel getDefaultLevel() {
        return HighlightDisplayLevel.ERROR;
    }

    @Nullable
    @Override
    @RequiredReadAction
    protected String buildErrorString(Object state, Object... args) {
        return ((LocalizeValue) args[0]).get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        @RequiredReadAction
        public void visitJSReferenceExpression(JSReferenceExpression reference) {
            super.visitJSReferenceExpression(reference);
            String name = reference.getReferencedName();
            if (reference.getQualifier() != null || name == null || !names(reference.getContainingFile()).contains(name)) {
                return;
            }
            PsiElement target = reference.resolve();
            if (target == null) {
                // not resolved before its declaration, in its own initializer
                target = lookup(reference, name);
                if (target == null) {
                    return;
                }
            }

            PsiElement declaration;
            LocalizeValue message;
            JSVariable variable = PsiTreeUtil.getParentOfType(target, JSVariable.class, false);
            if (variable != null) {
                JavaScriptFlowVariables.Kind kind = JavaScriptFlowVariables.kind(target);
                if (kind != JavaScriptFlowVariables.Kind.LET && kind != JavaScriptFlowVariables.Kind.CONST) {
                    return;
                }
                // the outermost variable of a destructuring
                for (JSVariable parent = variable; parent != null; parent = PsiTreeUtil.getParentOfType(parent, JSVariable.class, true)) {
                    variable = parent;
                }
                declaration = variable;
                message = InspectionJSLocalize.blockScopedVariableUsedBeforeDeclarationErrorString(name);
            }
            else if (isClass(target)) {
                declaration = target;
                message = InspectionJSLocalize.classUsedBeforeDeclarationErrorString(name);
            }
            else if (isEnum(target)) {
                declaration = target;
                message = InspectionJSLocalize.enumUsedBeforeDeclarationErrorString(name);
            }
            else {
                return;
            }

            if (isUsedBeforeDeclaration(reference, declaration)) {
                PsiElement nameElement = reference.getReferenceNameElement();
                registerError(nameElement != null ? nameElement : reference, message);
            }
        }
    }

    /**
     * The declaration of the name in the innermost scope around the usage declaring it: a variable, a class, an enum, a
     * function, a parameter - a var of a nested block is not seen
     */
    @RequiredReadAction
    private static @Nullable PsiElement lookup(PsiElement usage, String name) {
        for (PsiElement scope = usage.getParent(); scope != null; scope = scope.getParent()) {
            for (PsiElement child = scope.getFirstChild(); child != null; child = child.getNextSibling()) {
                PsiElement declaration = declaration(child, name);
                if (declaration != null) {
                    return declaration;
                }
            }
            if (scope instanceof PsiFile) {
                return null;
            }
        }
        return null;
    }

    /**
     * The element declares the name: a variable statement, a class, an enum, a function, a parameter, a TypeScript
     * statement or an export around one of them
     */
    @RequiredReadAction
    private static @Nullable PsiElement declaration(PsiElement element, String name) {
        if (element instanceof JSVarStatement || element instanceof JSParameterList) {
            for (PsiElement child = element.getFirstChild(); child != null; child = child.getNextSibling()) {
                if (child instanceof JSVariable variable) {
                    for (PsiElement target : JavaScriptFlowVariables.targets(variable)) {
                        if (name.equals(JavaScriptFlowVariables.name(target))) {
                            return target;
                        }
                    }
                }
            }
            return null;
        }
        if (element instanceof JSVariable variable && (element.getParent() instanceof JSLoopStatement || element.getParent() instanceof JSCatchBlock)) {
            // for (let x of ...), catch (e) of the TypeScript tree
            return name.equals(variable.getName()) ? variable : null;
        }
        if (element instanceof JSClass || element instanceof JSFunction || isEnum(element)) {
            PsiElement identifier = element instanceof PsiNameIdentifierOwner owner ? owner.getNameIdentifier() : null;
            if (identifier == null) {
                for (PsiElement child = element.getFirstChild(); child != null; child = child.getNextSibling()) {
                    if (PsiUtilCore.getElementType(child) == JSTokenTypes.IDENTIFIER) {
                        identifier = child;
                        break;
                    }
                }
            }
            return identifier != null && name.equals(identifier.getText()) ? element : null;
        }
        if (element instanceof JSStatement && element.getFirstChild() != null && !(element instanceof JSExpressionStatement) && !isScopeStatement(element)) {
            // a TypeScript statement around a declaration, an export
            for (PsiElement child = element.getFirstChild(); child != null; child = child.getNextSibling()) {
                PsiElement declaration = child.getFirstChild() != null ? declaration(child, name) : null;
                if (declaration != null) {
                    return declaration;
                }
            }
        }
        return null;
    }

    /**
     * A statement with a scope of its own - its declarations are not seen from out of it
     */
    private static boolean isScopeStatement(PsiElement element) {
        return element instanceof JSBlockStatement || element instanceof JSIfStatement || element instanceof JSLoopStatement || element instanceof JSSwitchStatement
            || element instanceof JSTryStatement || element instanceof JSLabeledStatement || element instanceof JSWithStatement;
    }

    @RequiredReadAction
    private static boolean isUsedBeforeDeclaration(PsiElement usage, PsiElement declaration) {
        PsiElement container = JavaScriptControlFlow.ownerOf(declaration);
        for (PsiElement parent = usage.getParent(); parent != null && parent != container; parent = parent.getParent()) {
            // runs later
            if ((parent instanceof JSFunction || JavaScriptControlFlow.isPropertyMethod(parent)) && !isImmediatelyInvoked(parent)
                || parent instanceof JSVariable field && isInstanceField(field)) {
                return false;
            }
        }
        if (usage.getTextRange().getStartOffset() < declaration.getTextRange().getStartOffset()) {
            return true;
        }
        // let x = x + 1
        JSExpression initializer = declaration instanceof JSVariable variable ? variable.getInitializer() : null;
        return initializer != null && PsiTreeUtil.isAncestor(initializer, usage, false);
    }

    /**
     * (function () { ... })() - runs at once
     */
    private static boolean isImmediatelyInvoked(PsiElement function) {
        if (!(function instanceof JSFunctionExpression)) {
            return false;
        }
        PsiElement callee = function;
        while (callee.getParent() instanceof JSParenthesizedExpression) {
            callee = callee.getParent();
        }
        return callee.getParent() instanceof JSCallExpression call && call.getMethodExpression() == callee;
    }

    /**
     * A property of a class without static - initialized when an instance is created
     */
    private static boolean isInstanceField(JSVariable field) {
        PsiElement body = field.getParent();
        if (field instanceof JSFunction || !(body instanceof JSClass || body != null && body.getParent() instanceof JSClass)) {
            return false;
        }
        // static in the attribute list - before the property in the TypeScript tree
        PsiElement attributes = PsiTreeUtil.getChildOfType(field, JSAttributeList.class);
        if (attributes == null) {
            PsiElement previous = PsiTreeUtil.skipWhitespacesAndCommentsBackward(field);
            attributes = previous instanceof JSAttributeList ? previous : null;
        }
        return attributes == null || attributes.getNode().findChildByType(JSTokenTypes.STATIC_KEYWORD) == null;
    }

    private static boolean isClass(PsiElement element) {
        return element instanceof JSClass && hasChild(element, JSTokenTypes.CLASS_KEYWORD);
    }

    private static boolean isEnum(PsiElement element) {
        return hasChild(element, JSTokenTypes.ENUM_KEYWORD);
    }

    private static boolean hasChild(PsiElement element, Object type) {
        for (PsiElement child = element.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (PsiUtilCore.getElementType(child) == type) {
                return true;
            }
        }
        return false;
    }

    /**
     * The names of the lets, consts, classes and enums of the file - another reference is not resolved
     */
    @RequiredReadAction
    private static Set<String> names(PsiFile file) {
        return LanguageCachedValueUtil.getCachedValue(file, () -> CachedValueProvider.Result.create(collectNames(file), PsiModificationTracker.MODIFICATION_COUNT));
    }

    @RequiredReadAction
    private static Set<String> collectNames(PsiFile file) {
        Set<String> names = new HashSet<>();
        for (PsiElement element : PsiTreeUtil.findChildrenOfAnyType(file, JSVariable.class, JSClass.class)) {
            if (element instanceof JSVariable variable && !(element instanceof JSFunction)) {
                JavaScriptFlowVariables.Kind kind = JavaScriptFlowVariables.kind(variable);
                if (kind == JavaScriptFlowVariables.Kind.LET || kind == JavaScriptFlowVariables.Kind.CONST) {
                    for (PsiElement leaf = PsiTreeUtil.firstChild(variable); leaf != null && PsiTreeUtil.isAncestor(variable, leaf, false); leaf = PsiTreeUtil.nextLeaf(leaf)) {
                        if (PsiUtilCore.getElementType(leaf) == JSTokenTypes.IDENTIFIER) {
                            names.add(leaf.getText());
                        }
                    }
                }
            }
            else if (element instanceof JSClass jsClass && isClass(jsClass) && jsClass.getName() != null) {
                names.add(jsClass.getName());
            }
        }
        // enum E { } - not an interface of the JavaScript tree
        for (PsiElement leaf = PsiTreeUtil.firstChild(file); leaf != null; leaf = PsiTreeUtil.nextLeaf(leaf)) {
            if (PsiUtilCore.getElementType(leaf) == JSTokenTypes.ENUM_KEYWORD) {
                PsiElement name = PsiTreeUtil.nextVisibleLeaf(leaf);
                if (name != null && PsiUtilCore.getElementType(name) == JSTokenTypes.IDENTIFIER) {
                    names.add(name.getText());
                }
            }
        }
        return names;
    }
}

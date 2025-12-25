/*
 * Copyright 2000-2005 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.intellij.lang.javascript.impl.generation;

import com.intellij.lang.javascript.formatter.JSCodeStyleSettings;
import com.intellij.lang.javascript.impl.validation.BaseCreateMethodsFix;
import com.intellij.lang.javascript.impl.validation.JSAnnotatingVisitor;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.application.Application;
import consulo.codeEditor.Editor;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.codeStyle.CodeStyleSettingsManager;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import consulo.ui.annotation.RequiredUIAccess;
import consulo.ui.ex.awt.NonFocusableCheckBox;
import consulo.util.lang.StringUtil;
import jakarta.annotation.Nonnull;

import javax.swing.*;
import java.util.*;

/**
 * @author Maxim.Mossienko
 * @since 2008-07-19
 */
class JavaScriptGenerateAccessorHandler extends BaseJSGenerateHandler {
    private GenerationMode mode;
    private JCheckBox myCreateBindableProperties;

    static enum GenerationMode {
        GETTERS,
        SETTERS,
        GETTERS_AND_SETTERS,
        CONSTRUCTOR,
        TOSTRING
    }

    JavaScriptGenerateAccessorHandler(GenerationMode _mode) {
        mode = _mode;
    }

    @Override
    protected LocalizeValue getTitle() {
        return mode == GenerationMode.GETTERS
            ? JavaScriptLocalize.generateGetterFieldsChooserTitle()
            : mode == GenerationMode.SETTERS
            ? JavaScriptLocalize.generateSetterFieldsChooserTitle()
            : mode == GenerationMode.GETTERS_AND_SETTERS
            ? JavaScriptLocalize.generateGetterSetterChooserTitle()
            : mode == GenerationMode.TOSTRING
            ? JavaScriptLocalize.generateToStringChooserTitle()
            : JavaScriptLocalize.generateConstructorFieldsChooserTitle();
    }

    @Override
    protected void appendOwnOptions(List<JComponent> jComponentList) {
        super.appendOwnOptions(jComponentList);
        if (mode == GenerationMode.GETTERS || mode == GenerationMode.GETTERS_AND_SETTERS || mode == GenerationMode.SETTERS) {
            if (!Application.get().isUnitTestMode()) {
                myCreateBindableProperties = new NonFocusableCheckBox(JavaScriptLocalize.generateGetterFieldsBindableProperties().get());
                jComponentList.add(myCreateBindableProperties);
            }
        }
    }

    @Override
    protected BaseCreateMethodsFix createFix(JSClass jsClass) {
        if (mode == GenerationMode.GETTERS_AND_SETTERS) {
            return new BaseCreateMethodsFix<JSVariable>(jsClass) {
                private boolean toCreateBindableProperties = myCreateBindableProperties != null
                    ? myCreateBindableProperties.isSelected()
                    : jsClass.getApplication().isUnitTestMode();
                final MyBaseCreateMethodsFix generateGetterFix =
                    new MyBaseCreateMethodsFix(GenerationMode.GETTERS, jsClass, toCreateBindableProperties);
                final MyBaseCreateMethodsFix generateSetterFix =
                    new MyBaseCreateMethodsFix(GenerationMode.SETTERS, jsClass, toCreateBindableProperties);

                @Override
                @RequiredUIAccess
                @RequiredWriteAction
                public void invoke(@Nonnull Project project, Editor editor, PsiFile file) throws IncorrectOperationException {
                    evalAnchor(editor, file);

                    for (JSVariable e : getElementsToProcess()) {
                        anchor = doAddOneMethod(project, generateGetterFix.buildFunctionText(e), anchor);
                        anchor = doAddOneMethod(project, generateSetterFix.buildFunctionText(e), anchor);
                    }
                }
            };
        }
        else if (mode == GenerationMode.CONSTRUCTOR) {
            return new BaseCreateMethodsFix<JSVariable>(jsClass) {
                @Override
                @RequiredUIAccess
                @RequiredWriteAction
                public void invoke(@Nonnull Project project, Editor editor, PsiFile file) throws IncorrectOperationException {
                    JSCodeStyleSettings codeStyleSettings =
                        CodeStyleSettingsManager.getSettings(project).getCustomSettings(JSCodeStyleSettings.class);
                    evalAnchor(editor, file);
                    StringBuilder functionText = new StringBuilder("public function ").append(jsClass.getName()).append("(");
                    StringBuilder initialization = new StringBuilder();
                    boolean first = true;
                    String semicolon = JSChangeUtil.getSemicolon(project);

                    Set<JSVariable> toProcess = getElementsToProcess();
                    Iterator<JSVariable> variableIterator = toProcess.iterator();
                    boolean hadSuperClassConstructorInitializationBefore = false;

                    while (variableIterator.hasNext()) {
                        JSVariable var = variableIterator.next();
                        if (!first) {
                            functionText.append(", ");
                        }

                        first = false;

                        String name = var.getName();
                        String parameterName = transformVarNameToAccessorName(name, codeStyleSettings);

                        String typeString = var.getTypeString();
                        functionText.append(parameterName).append(typeString != null ? ":" + typeString : "");

                        if (JSResolveUtil.findParent(var) == jsClass) {
                            if (hadSuperClassConstructorInitializationBefore) {
                                initialization.append(")").append(semicolon).append("\n");
                                hadSuperClassConstructorInitializationBefore = false;
                            }
                            initialization.append(parameterName.equals(name) ? "this." : "")
                                .append(name)
                                .append(" = ")
                                .append(parameterName)
                                .append(semicolon)
                                .append("\n");
                        }
                        else {
                            if (hadSuperClassConstructorInitializationBefore) {
                                initialization.append(", ");
                            }
                            else {
                                initialization.append("super(");
                            }
                            initialization.append(parameterName);
                            hadSuperClassConstructorInitializationBefore = true;
                        }
                    }

                    if (hadSuperClassConstructorInitializationBefore) {
                        initialization.append(")").append(semicolon).append("\n");
                    }
                    functionText.append(") {\n")
                        .append(initialization)
                        .append("}");
                    doAddOneMethod(project, functionText.toString(), anchor);
                }

                @Override
                @RequiredReadAction
                public Set<JSVariable> getElementsToProcess() {
                    LinkedHashSet<JSVariable> vars = new LinkedHashSet<>();
                    JSFunction nontrivialSuperClassConstructor = JSAnnotatingVisitor.getNontrivialSuperClassConstructor(jsClass);

                    if (nontrivialSuperClassConstructor != null) {
                        vars.addAll(Arrays.asList(nontrivialSuperClassConstructor.getParameterList().getParameters()));
                    }
                    vars.addAll(super.getElementsToProcess());
                    return vars;
                }
            };
        }
        else if (mode == GenerationMode.TOSTRING) {
            return new BaseCreateMethodsFix<JSVariable>(jsClass) {
                @Override
                @RequiredUIAccess
                @RequiredWriteAction
                public void invoke(@Nonnull Project project, Editor editor, PsiFile file) throws IncorrectOperationException {
                    evalAnchor(editor, file);

                    boolean[] needOverride = new boolean[1];
                    JSResolveUtil.processOverrides(
                        jsClass,
                        (processor, scope, className) -> {
                            needOverride[0] = !"Object".equals(className);
                            return false;
                        },
                        "toString",
                        null,
                        myJsClass
                    );

                    StringBuilder functionText = new StringBuilder().append("public ")
                        .append(needOverride[0] ? "override " : "")
                        .append("function toString():String {\nreturn ")
                        .append(needOverride[0] ? "super.toString() + \"" : "\"" + jsClass.getName())
                        .append("{");
                    String semicolon = JSChangeUtil.getSemicolon(project);

                    boolean first = true;

                    for (JSVariable var : getElementsToProcess()) {
                        if (!first) {
                            functionText.append(" + \",");
                        }
                        first = false;

                        functionText.append(var.getName()).append("=\" + String(").append(var.getName()).append(")");
                    }

                    functionText.append("+\"}\"").append(semicolon).append("\n}");
                    doAddOneMethod(project, functionText.toString(), anchor);
                }
            };
        }

        return new MyBaseCreateMethodsFix(
            mode,
            jsClass,
            myCreateBindableProperties != null && myCreateBindableProperties.isSelected()
        );
    }

    @Override
    @RequiredReadAction
    protected void collectCandidates(JSClass clazz, Collection<JSNamedElementNode> candidates) {
        final LinkedHashMap<String, JSNamedElement> candidatesMap = new LinkedHashMap<>();
        final JSCodeStyleSettings codeStyleSettings =
            CodeStyleSettingsManager.getSettings(clazz.getProject()).getCustomSettings(JSCodeStyleSettings.class);
        ResolveProcessor processor = new ResolveProcessor(null) {
            {
                setToProcessMembers(true);
                setToProcessHierarchy(false);
                setLocalResolve(true);
            }

            @Override
            @RequiredReadAction
            public boolean execute(@Nonnull PsiElement element, ResolveState state) {
                if (element instanceof JSVariable variable) {
                    if (variable.isConst()) {
                        return true;
                    }
                    String name = variable.getName();
                    String accessorName = transformVarNameToAccessorName(name, codeStyleSettings);
                    if (/*!name.equals(accessorName) &&*/ !candidatesMap.containsKey(accessorName)) {
                        candidatesMap.put(accessorName, variable);
                    }
                }
                else if (element instanceof JSFunction function
                    && (mode == GenerationMode.GETTERS && function.isGetProperty()
                    || mode == GenerationMode.SETTERS && function.isSetProperty())) {
                    candidatesMap.put(function.getName(), function);
                }
                return true;
            }
        };

        clazz.processDeclarations(processor, ResolveState.initial(), clazz, clazz);
        for (JSNamedElement n : candidatesMap.values()) {
            if (n instanceof JSVariable variable) {
                candidates.add(new JSNamedElementNode(variable));
            }
        }
    }

    private static class MyBaseCreateMethodsFix extends BaseCreateMethodsFix<JSVariable> {
        private GenerationMode myMode;
        private JSCodeStyleSettings codeStyleSettings;
        private boolean bindableProperties;
        private static final String PARAMETER_NAME = "value";

        public MyBaseCreateMethodsFix(GenerationMode mode, JSClass jsClass, boolean _bindableProperties) {
            super(jsClass);
            this.myMode = mode;
            codeStyleSettings = CodeStyleSettingsManager.getSettings(jsClass.getProject()).getCustomSettings(JSCodeStyleSettings.class);
            bindableProperties = _bindableProperties;
        }

        @Override
        @RequiredReadAction
        protected String buildFunctionBodyText(String retType, JSParameterList parameterList, JSVariable func) {
            String semicolon = codeStyleSettings.USE_SEMICOLON_AFTER_STATEMENT ? ";" : "";
            String varName = func.getName();
            if (myMode == GenerationMode.SETTERS) {
                String checkNeedEvent = "";
                String dispatchEvent = "";
                if (bindableProperties) {
                    String eventName = getEventName(transformVarNameToAccessorName(varName, codeStyleSettings));
                    dispatchEvent = "\ndispatchEvent(new Event(\"" + eventName + "\"))" + semicolon;
                    checkNeedEvent = "if(" + varName + "==" + PARAMETER_NAME + ") return" + semicolon + "\n";
                }
                return "{\n" + checkNeedEvent + varName + "=" + PARAMETER_NAME + semicolon + dispatchEvent + "\n}";
            }
            else if (myMode == GenerationMode.GETTERS) {
                return "{\nreturn " + varName + semicolon + "\n}";
            }
            return " {}";
        }

        @Override
        @RequiredReadAction
        protected String buildFunctionAttrText(String attrText, JSAttributeList attributeList, JSVariable function) {
            StringBuilder baseText = new StringBuilder();

            if (bindableProperties && myMode == GenerationMode.GETTERS) {
                baseText.append("[Bindable(event=\"")
                    .append(getEventName(transformVarNameToAccessorName(function.getName(), codeStyleSettings)))
                    .append("\")]\n");
            }

            baseText.append("public");
            if (attributeList != null && attributeList.hasModifier(JSAttributeList.ModifierType.STATIC)) {
                baseText.append(" static");
            }

            return baseText.toString();
        }

        private static String getEventName(String name) {
            return name + "Changed";
        }

        @Override
        @RequiredReadAction
        protected String buildFunctionKind(JSVariable fun) {
            if (myMode == GenerationMode.GETTERS) {
                return "get ";
            }
            if (myMode == GenerationMode.SETTERS) {
                return "set ";
            }
            return super.buildFunctionKind(fun);
        }

        @Override
        protected String buildReturnType(String typeString) {
            if (myMode == GenerationMode.SETTERS) {
                return "void";
            }
            return super.buildReturnType(typeString);
        }

        @Override
        @RequiredReadAction
        protected String buildName(JSVariable fun) {
            return transformVarNameToAccessorName(super.buildName(fun), codeStyleSettings);
        }

        @Override
        @RequiredReadAction
        protected String buildParameterList(JSParameterList parameterList, JSVariable fun) {
            if (myMode == GenerationMode.SETTERS) {
                String s = fun.getTypeString();
                return "(" + PARAMETER_NAME + (s != null ? ":" + s : "") + ")";
            }
            return (parameterList != null ? parameterList.getText() : "()");
        }
    }

    private static String transformVarNameToAccessorName(String s, JSCodeStyleSettings codeStyleSettings) {
        if (StringUtil.startsWith(s, codeStyleSettings.FIELD_PREFIX)) {
            s = s.substring(codeStyleSettings.FIELD_PREFIX.length());
        }
        return codeStyleSettings.PROPERTY_PREFIX + s;
    }

    @Override
    protected boolean canHaveEmptySelectedElements() {
        return mode == GenerationMode.CONSTRUCTOR;
    }
}


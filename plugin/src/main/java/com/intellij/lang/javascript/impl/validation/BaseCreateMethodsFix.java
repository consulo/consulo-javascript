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

package com.intellij.lang.javascript.impl.validation;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.impl.flex.ImportUtils;
import com.intellij.lang.javascript.impl.generation.JSNamedElementNode;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.lang.javascript.psi.resolve.JSImportHandlingUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.codeEditor.Editor;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiWhiteSpace;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;
import consulo.util.lang.StringUtil;
import jakarta.annotation.Nonnull;

import java.util.*;

/**
 * @author Maxim.Mossienko
 * @since 2008-07-17
 */
public abstract class BaseCreateMethodsFix<T extends JSNamedElement & JSAttributeListOwner> {
    private final Set<T> elementsToProcess = new LinkedHashSet<>();
    protected final JSClass myJsClass;
    protected PsiElement anchor;

    public BaseCreateMethodsFix(JSClass jsClass) {
        myJsClass = jsClass;
    }

    @RequiredWriteAction
    public void invoke(@Nonnull Project project, Editor editor, PsiFile file) throws IncorrectOperationException {
        evalAnchor(editor, file);
        for (T e : getElementsToProcess()) {
            anchor = doAddOneMethod(project, buildFunctionText(e), anchor);
        }
    }

    @RequiredReadAction
    protected void evalAnchor(Editor editor, PsiFile file) {
        anchor = null;
        PsiElement at = file.findElementAt(editor.getCaretModel().getOffset());
        PsiElement parent = at != null ? at.getParent() : null;

        if (parent == myJsClass || (parent instanceof JSFile jsFile
            && myJsClass.getParent().getContainingFile() == jsFile.getContext().getContainingFile())) {

            ASTNode atNode = at.getNode();
            if (atNode.getElementType() == JSTokenTypes.RBRACE) {
                return;
            }

            for (ASTNode node = atNode; node != null; node = node.getTreeNext()) {
                if (node.getElementType() == JSTokenTypes.LBRACE) {
                    return;
                }
            }
            anchor = at;
        }
    }

    @RequiredWriteAction
    protected PsiElement doAddOneMethod(Project project, String functionText, PsiElement anchor)
        throws IncorrectOperationException {
        if (StringUtil.isNotEmpty(functionText)) {
            PsiElement element = JSChangeUtil.createJSTreeFromText(project, functionText).getPsi();
            if (element instanceof PsiWhiteSpace whiteSpace) {
                element = whiteSpace.getNextSibling();
            }

            boolean defaultAdd = true;

            if (anchor != null && anchor.isValid()) {
                String anchorText = anchor instanceof PsiWhiteSpace ? anchor.getText() : "";

                if (!anchorText.contains("<![CDATA[") && !anchorText.contains("]]>")) {
                    defaultAdd = false;
                    anchor = anchor.getParent().addAfter(element, anchor);
                }
            }

            if (defaultAdd) {
                anchor = myJsClass.add(element);
            }
        }

        return anchor;
    }

    @RequiredWriteAction
    public String buildFunctionText(T fun) {
        JSAttributeList attributeList = fun.getAttributeList();
        String attrText = attributeList != null ? attributeList.getText() : "";

        attrText = buildFunctionAttrText(attrText, attributeList, fun);

        JSFunction function = fun instanceof JSFunction jsFun ? jsFun : null;
        JSVariable var = fun instanceof JSVariable jsVar ? jsVar : null;
        assert var != null || function != null;

        JSParameterList parameterList = (function != null) ? function.getParameterList() : null;
        String typeString = importType(function != null ? function.getReturnTypeString() : var.getTypeString(), fun);
        StringBuilder functionText = new StringBuilder(attrText);
        if (!functionText.isEmpty()) {
            functionText.append(" ");
        }
        functionText.append("function ");

        functionText.append(buildFunctionKind(fun));

        functionText.append(buildName(fun)).append(" ").append(buildParameterList(parameterList, fun));
        if (typeString != null) {
            functionText.append(":").append(buildReturnType(typeString));
        }

        functionText.append(buildFunctionBodyText(typeString, parameterList, fun));
        return functionText.toString();
    }

    protected String buildReturnType(String typeString) {
        return typeString;
    }

    @RequiredWriteAction
    protected String importType(String s, T fun) {
        if (s == null) {
            return null;
        }
        if (fun instanceof JSFunction function) {
            String resolvedTypeName = JSImportHandlingUtil.resolveTypeName(s, function);

            if (!resolvedTypeName.equals(s)) {
                ImportUtils.doImport(myJsClass, resolvedTypeName);
            }
        }
        return s;
    }

    @RequiredWriteAction
    protected String buildParameterList(JSParameterList parameterList, T fun) {
        if (parameterList != null) {
            for (JSParameter param : parameterList.getParameters()) {
                String s = param.getTypeString();
                if (s != null) {
                    importType(s, fun);
                }
            }
        }
        return (parameterList != null ? parameterList.getText() : "()");
    }

    @RequiredReadAction
    protected String buildName(T fun) {
        return fun.getName();
    }

    @RequiredReadAction
    protected String buildFunctionKind(T fun) {
        if (fun instanceof JSFunction function) {
            if (function.isGetProperty()) {
                return "get ";
            }
            else if (function.isSetProperty()) {
                return "set ";
            }
        }
        return "";
    }

    protected String buildFunctionBodyText(String retType, JSParameterList parameterList, T func) {
        return " {}";
    }

    protected String buildFunctionAttrText(String attrText, JSAttributeList attributeList, T function) {
        return attrText.replace("native", "").trim();
    }

    public void addElementToProcess(T function) {
        elementsToProcess.add(function);
    }

    @SuppressWarnings("unchecked")
    public void addElementsToProcessFrom(Collection<JSNamedElementNode> selectedElements) {
        for (JSNamedElementNode el : selectedElements) {
            addElementToProcess((T)el.getPsiElement());
        }
    }

    @SuppressWarnings("unchecked")
    public Set<T> getElementsToProcess() {
        T[] objects = (T[])elementsToProcess.toArray(new JSNamedElement[elementsToProcess.size()]);
        Comparator<T> tComparator = (o1, o2) -> o1.getTextOffset() - o2.getTextOffset();

        int size = elementsToProcess.size();
        LinkedHashSet<T> result = new LinkedHashSet<>(size);
        List<T> objectsFromSameFile = new ArrayList<>();
        PsiFile containingFile = null;

        for (int i = 0; i < size; ++i) {
            T object = objects[i];
            PsiFile currentContainingFile = object.getContainingFile();

            if (currentContainingFile != containingFile) {
                if (containingFile != null) {
                    Collections.sort(objectsFromSameFile, tComparator);
                    result.addAll(objectsFromSameFile);
                    objectsFromSameFile.clear();
                }
                containingFile = currentContainingFile;
            }

            objectsFromSameFile.add(object);
        }

        Collections.sort(objectsFromSameFile, tComparator);
        result.addAll(objectsFromSameFile);

        elementsToProcess.clear();
        elementsToProcess.addAll(result);
        return elementsToProcess;
    }
}

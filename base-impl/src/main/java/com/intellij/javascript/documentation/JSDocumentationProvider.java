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

package com.intellij.javascript.documentation;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.index.JavaScriptIndex;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSPsiImplUtils;
import com.intellij.lang.javascript.psi.resolve.BaseJSSymbolProcessor;
import com.intellij.lang.javascript.psi.resolve.JSImportHandlingUtil;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.ast.ASTNode;
import consulo.language.editor.documentation.CodeDocumentationProvider;
import consulo.language.editor.documentation.DocumentationProvider;
import consulo.language.editor.documentation.LanguageDocumentationProvider;
import consulo.language.psi.*;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.navigation.ItemPresentation;
import consulo.navigation.NavigationItem;
import consulo.project.Project;
import consulo.util.lang.Pair;
import consulo.util.lang.StringUtil;
import consulo.util.lang.ref.SimpleReference;
import consulo.xml.psi.xml.XmlToken;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.io.File;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Maxim.Mossienko
 * @since 2005-11-04
 */
@ExtensionImpl
public class JSDocumentationProvider implements CodeDocumentationProvider, LanguageDocumentationProvider {
    private DocumentationProvider cssProvider;
    private static final String OBJECT_NAME = "Object";
    protected static final String SEE_PLAIN_TEXT_CHARS = "\t \"-\\/<>*";

    protected static final String PACKAGE = "package";
    protected static final String HTML_EXTENSION = ".html";
    protected static String PACKAGE_FILE = PACKAGE + HTML_EXTENSION;

    protected static final Map<String, String> DOCUMENTED_ATTRIBUTES;

    static {
        DOCUMENTED_ATTRIBUTES = new HashMap<>();
        DOCUMENTED_ATTRIBUTES.put("Event", "event:");
        DOCUMENTED_ATTRIBUTES.put("Style", "style:");
        DOCUMENTED_ATTRIBUTES.put("Effect", "effect:");
    }

    private DocumentationProvider getCssProvider(Project project) throws Exception {
        if (cssProvider == null) {
            Class<?> aClass = Class.forName("com.intellij.psi.css.impl.util.CssDocumentationProvider");
            cssProvider = (DocumentationProvider)aClass.getConstructor(new Class[]{}).newInstance(new Object[]{});
        }

        return cssProvider;
    }

    @Override
    @Nullable
    @RequiredReadAction
    public String getQuickNavigateInfo(PsiElement element, PsiElement element2) {
        if (element instanceof JSFunction function) {
            if (function.isConstructor() && function.getParent() instanceof JSClass jsClass) {
                return createQuickNavigateForClazz(jsClass);
            }
            return createQuickNavigateForFunction(function);
        }
        else if (element instanceof JSClass jsClass) {
            return createQuickNavigateForClazz(jsClass);
        }
        else if (element instanceof JSVariable variable) {
            return createQuickNavigateForVariable(variable);
        }
        else if (element instanceof JSAttributeNameValuePair attributeNameValuePair) {
            return createQuickNavigateForAnnotationDerived(attributeNameValuePair);
        }
        else if (element instanceof XmlToken xmlToken) {
            BaseJSSymbolProcessor.TagContextBuilder builder = new BaseJSSymbolProcessor.TagContextBuilder(xmlToken, "XmlTag");
            return StringUtil.stripQuotesAroundValue(xmlToken.getText()) + ":" + builder.typeName;
        }
        else if (element instanceof JSNamespaceDeclaration namespaceDeclaration) {
            return createQuickNavigateForNamespace(namespaceDeclaration);
        }

        return null;
    }

    @RequiredReadAction
    private static String createQuickNavigateForAnnotationDerived(PsiElement element) {
        JSAttributeNameValuePair valuePair = (JSAttributeNameValuePair)element;
        JSAttribute parent = (JSAttribute)valuePair.getParent();
        StringBuilder builder = new StringBuilder();
        JSClass clazz = PsiTreeUtil.getParentOfType(valuePair, JSClass.class);
        appendParentInfo(clazz != null ? clazz : parent.getContainingFile(), builder, parent);
        builder.append(parent.getName()).append(" ").append(valuePair.getSimpleValue());
        return builder.toString();
    }

    @Nullable
    @RequiredReadAction
    private static String createQuickNavigateForFunction(JSFunction function) {
        PsiElement parent = JSResolveUtil.findParent(function);
        StringBuilder result = new StringBuilder();

        appendParentInfo(parent, result, function);

        appendAttrList(function, result);
        boolean get = function.isGetProperty();
        boolean set = function.isSetProperty();

        result.append(get || set ? "property " : "function ");
        result.append(function.getName());

        if (!get && !set) {
            result.append('(');
            JSParameterList jsParameterList = function.getParameterList();

            if (jsParameterList != null) {
                int start = result.length();

                for (JSParameter p : jsParameterList.getParameters()) {
                    if (start != result.length()) {
                        result.append(", ");
                    }
                    result.append(p.getName());
                    appendVarType(p, result);
                }
            }

            result.append(')');
        }

        String varType = null;

        if (get || !set) {
            varType = JSImportHandlingUtil.resolveTypeName(function.getReturnTypeString(), function);
        }
        else {
            JSParameterList jsParameterList = function.getParameterList();

            if (jsParameterList != null) {
                JSParameter[] jsParameters = jsParameterList.getParameters();
                if (jsParameters != null && jsParameters.length > 0) {
                    varType = JSImportHandlingUtil.resolveTypeName(jsParameters[0].getTypeString(), function);
                }
            }
        }

        if (varType != null) {
            result.append(':').append(varType);
        }
        return result.toString();
    }

    @RequiredReadAction
    private static void appendParentInfo(PsiElement parent, StringBuilder builder, PsiNamedElement element) {
        if (parent instanceof JSClass jsClass) {
            builder.append(jsClass.getQualifiedName()).append("\n");
        }
        else if (parent instanceof JSPackageStatement packageStatement) {
            builder.append(packageStatement.getQualifiedName()).append("\n");
        }
        else if (parent instanceof JSFile) {
            if (parent.getContext() != null) {
                String mxmlPackage = JSResolveUtil.findPackageForMxml(parent);
                if (mxmlPackage != null) {
                    builder.append(mxmlPackage)
                        .append(mxmlPackage.length() > 0 ? "." : "")
                        .append(parent.getContext().getContainingFile().getName())
                        .append("\n");
                }
            }
            else {
                boolean foundQualified = false;

                if (element instanceof JSNamedElement namedElement) {
                    PsiElement node = namedElement.getNameIdentifier();
                    if (node != null) {
                        String s = node.getText();
                        int i = s.lastIndexOf('.');
                        if (i != -1) {
                            builder.append(s.substring(0, i)).append("\n");
                            foundQualified = true;
                        }
                    }
                }
                if (!foundQualified) {
                    builder.append(parent.getContainingFile().getName()).append("\n");
                }
            }
        }
    }

    @Nullable
    @RequiredReadAction
    private static String createQuickNavigateForVariable(JSVariable variable) {
        PsiElement parent = JSResolveUtil.findParent(variable);
        StringBuilder result = new StringBuilder();

        appendParentInfo(parent, result, variable);

        appendAttrList(variable, result);
        result.append(variable.isConst() ? "const " : "var ");
        result.append(variable.getName());
        appendVarType(variable, result);

        JSExpression initializer = variable.getInitializer();
        if (initializer != null) {
            result.append(" = ");
            if (initializer instanceof JSLiteralExpression) {
                result.append(initializer.getText());
            }
            else if (initializer instanceof JSObjectLiteralExpression) {
                result.append("{...}");
            }
            else {
                result.append("...");
            }
        }
        return result.toString();
    }

    private static void appendVarType(JSVariable variable, StringBuilder builder) {
        String varType = variable.getTypeString();
        if (varType != null) {
            builder.append(':').append(varType);
        }
    }

    @Nullable
    @RequiredReadAction
    private static String createQuickNavigateForClazz(JSClass jsClass) {
        String qName = jsClass.getQualifiedName();
        if (qName == null) {
            return null;
        }
        StringBuilder result = new StringBuilder();
        String packageName = StringUtil.getPackageName(qName);
        if (packageName.length() > 0) {
            result.append(packageName).append("\n");
        }

        appendAttrList(jsClass, result);
        result.append(jsClass.isInterface() ? "interface" : "class");

        String name = jsClass.getName();
        result.append(" ").append(name);

        String s = generateReferenceTargetList(jsClass.getExtendsList(), packageName);
        if (s == null && !OBJECT_NAME.equals(name)) {
            s = OBJECT_NAME;
        }
        if (s != null) {
            result.append(" extends ").append(s);
        }

        s = generateReferenceTargetList(jsClass.getImplementsList(), packageName);
        if (s != null) {
            result.append("\nimplements ").append(s);
        }

        return result.toString();
    }

    @Nullable
    @RequiredReadAction
    private static String createQuickNavigateForNamespace(JSNamespaceDeclaration ns) {
        String qName = ns.getQualifiedName();
        if (qName == null) {
            return null;
        }
        StringBuilder result = new StringBuilder();
        String packageName = StringUtil.getPackageName(qName);
        if (packageName.length() > 0) {
            result.append(packageName).append("\n");
        }

        result.append("namespace");

        result.append(" ").append(ns.getName());

        String s = ns.getInitialValueString();
        if (s != null) {
            result.append(" = ").append(s);
        }
        return result.toString();
    }

    @RequiredReadAction
    private static void appendAttrList(JSAttributeListOwner jsClass, StringBuilder result) {
        JSAttributeList attributeList = jsClass.getAttributeList();
        if (attributeList != null) {
            if (attributeList.hasModifier(JSAttributeList.ModifierType.OVERRIDE)) {
                result.append("override ");
            }
            JSAttributeList.AccessType type = attributeList.getAccessType();
            String ns = attributeList.getNamespace();
            if (type != JSAttributeList.AccessType.PACKAGE_LOCAL || ns == null) {
                result.append(type.toString().toLowerCase());
            }
            else {
                result.append(ns);
            }

            result.append(" ");

            if (attributeList.hasModifier(JSAttributeList.ModifierType.STATIC)) {
                result.append("static ");
            }
            if (attributeList.hasModifier(JSAttributeList.ModifierType.FINAL)) {
                result.append("final ");
            }
            if (attributeList.hasModifier(JSAttributeList.ModifierType.DYNAMIC)) {
                result.append("dynamic ");
            }
            if (attributeList.hasModifier(JSAttributeList.ModifierType.NATIVE)) {
                result.append("native ");
            }
        }
    }

    @Nullable
    @RequiredReadAction
    private static String generateReferenceTargetList(@Nullable JSReferenceList implementsList, @Nonnull String packageName) {
        if (implementsList == null) {
            return null;
        }
        StringBuilder result = null;

        String[] referenceExpressionTexts = implementsList.getReferenceTexts();

        for (String refExprText : referenceExpressionTexts) {
            refExprText = JSImportHandlingUtil.resolveTypeName(refExprText, implementsList);
            if (result == null) {
                result = new StringBuilder();
            }
            else {
                result.append(",");
            }

            String referencedPackageName = StringUtil.getPackageName(refExprText);
            result.append(referencedPackageName.equals(packageName)
                ? refExprText.substring(refExprText.lastIndexOf('.') + 1)
                : refExprText);
        }
        return result == null ? null : result.toString();
    }

    @Override
    public List<String> getUrlFor(PsiElement element, PsiElement originalElement) {
        String possibleCssName = findPossibleCssName(element);

        if (possibleCssName != null) {
            try {
                DocumentationProvider documentationProvider = getCssProvider(element.getProject());
                Method method = documentationProvider.getClass().getMethod("getUrlFor", new Class[]{String.class});
                Object o = method.invoke(null, new Object[]{possibleCssName});

                if (o instanceof String) {
                    return Collections.singletonList((String)o);
                }
            }
            catch (Exception ignored) {
            }
        }
        return null;
    }

    @Override
    @RequiredReadAction
    public String generateDoc(PsiElement _element, PsiElement originalElement) {
        if (_element instanceof JSReferenceExpression expression) {
            StringBuilder buffer = null;

            // ambigious reference
            for (ResolveResult r : expression.multiResolve(false)) {
                if (buffer == null) {
                    buffer = new StringBuilder();
                }
                PsiElement element = r.getElement();
                ItemPresentation presentation = ((NavigationItem)element).getPresentation();

                JSDocumentationUtils.appendHyperLinkToElement(
                    element,
                    expression.getReferencedName(),
                    buffer,
                    presentation.getPresentableText(),
                    presentation.getLocationString()
                );
                buffer.append("<br/>\n");
            }
            return buffer != null ? buffer.toString() : null;
        }

        _element = _element.getNavigationElement();
        PsiElement element = findElementForWhichPreviousCommentWillBeSearched(_element);

        boolean parameterDoc = element instanceof JSParameter;
        if (parameterDoc) {
            element = findElementForWhichPreviousCommentWillBeSearched(PsiTreeUtil.getParentOfType(element, JSFunction.class));
        }

        if (element != null) {
            PsiElement docComment =
                JSDocumentationUtils.findDocComment(element, _element instanceof JSAttributeNameValuePair ? originalElement : null);

            if (docComment != null) {
                docComment = findFirstDocComment(docComment);
                element = findTargetElement(_element, element);
                JSDocumentationBuilder builder = new JSDocumentationBuilder(element, originalElement);
                JSDocumentationUtils.processDocumentationTextFromComment(docComment.getNode(), builder);

                return parameterDoc ? builder.getParameterDoc(((JSParameter)_element).getName()) : builder.getDoc();
            }

            element = findTargetElement(_element, element);

            if (element instanceof JSFunction) {
                ASTNode initialComment = JSDocumentationUtils.findLeadingCommentInFunctionBody(element);

                if (initialComment != null) {
                    JSDocumentationBuilder builder = new JSDocumentationBuilder(element, originalElement);
                    JSDocumentationUtils.processDocumentationTextFromComment(initialComment, builder);
                    return builder.getDoc();
                }
            }
        }

        String possibleCssName = findPossibleCssName(_element);
        if (possibleCssName != null) {
            try {
                DocumentationProvider documentationProvider = getCssProvider(_element.getProject());
                Method declaredMethod =
                    documentationProvider.getClass().getDeclaredMethod("generateDoc", String.class, PsiElement.class);
                Object o = declaredMethod.invoke(null, possibleCssName, null);

                if (o instanceof String) {
                    return (String)o;
                }
            }
            catch (Exception ignored) {
            }
        }

        return null;
    }

    @RequiredReadAction
    private static PsiElement findTargetElement(PsiElement _element, PsiElement element) {
        if (_element instanceof JSDefinitionExpression definition) {
            if (definition.getParent() instanceof JSAssignmentExpression assignment) {
                JSExpression rOperand = assignment.getROperand();
                element = rOperand instanceof JSFunctionExpression ? rOperand : definition;
            }
        }
        else if (_element instanceof JSFunctionExpression function) {
            element = function;
        }
        else if (_element instanceof JSProperty property) {
            if (property.getValue() instanceof JSFunction function) {
                element = function;
            }
        }
        else if (_element instanceof JSVariable variable) {
            if (variable instanceof JSParameter parameter) {
                return PsiTreeUtil.getParentOfType(parameter, JSFunction.class);
            }
            element = variable;
        }
        else if (_element instanceof JSAttributeNameValuePair nameValuePair) {
            return nameValuePair;
        }
        return element;
    }

    @RequiredReadAction
    private static PsiElement findFirstDocComment(PsiElement docComment) {
        if (docComment.getNode().getElementType() == JSTokenTypes.END_OF_LINE_COMMENT) {
            while (true) {
                PsiElement prev = docComment.getPrevSibling();
                if (prev instanceof PsiWhiteSpace whiteSpace) {
                    prev = whiteSpace.getPrevSibling();
                }
                if (prev == null) {
                    break;
                }
                if (prev.getNode().getElementType() != JSTokenTypes.END_OF_LINE_COMMENT) {
                    break;
                }
                docComment = prev;
            }
        }
        return docComment;
    }

    private static String findPossibleCssName(PsiElement _element) {
        if (_element instanceof JSDefinitionExpression definition) {
            JSExpression expression = definition.getExpression();

            if (expression instanceof JSReferenceExpression reference) {
                String text = reference.getReferencedName();
                if (text == null) {
                    return null;
                }
                StringBuilder sb = new StringBuilder(text.length());

                for (int i = 0; i < text.length(); ++i) {
                    char ch = text.charAt(i);

                    if (Character.isUpperCase(ch)) {
                        sb.append('-').append(Character.toLowerCase(ch));
                    }
                    else {
                        sb.append(ch);
                    }
                }

                return sb.toString();
            }
        }
        return null;
    }

    @Override
    public PsiElement getDocumentationElementForLookupItem(PsiManager psiManager, Object object, PsiElement element) {
        PsiElement psiElement = findElementForWhichPreviousCommentWillBeSearched(object);
        if (psiElement != null && JSDocumentationUtils.findDocComment(psiElement) != null) {
            return psiElement;
        }
        if (object instanceof PsiElement objPsiElement) {
            return objPsiElement;
        }
        return null;
    }

    @RequiredReadAction
    public static PsiElement findElementForWhichPreviousCommentWillBeSearched(Object object) {
        if (object instanceof JSFunction function) {
            PsiElement psiElement = function;
            PsiElement parent = psiElement.getParent();
            if (parent instanceof JSNewExpression newExpr) {
                parent = newExpr.getParent();
            }
            if (parent instanceof JSProperty property) {
                psiElement = property;
            }
            else if (parent instanceof JSAssignmentExpression assignment) {
                psiElement = assignment.getParent();
            }

            if (function.isSetProperty() || function.isGetProperty()) {
                for (PsiElement el = function.getPrevSibling(); el != null; el = el.getPrevSibling()) {
                    if (!(el instanceof PsiWhiteSpace) && !(el instanceof PsiComment)) {
                        if (el instanceof JSFunction prevFunction) {
                            String name = prevFunction.getName();

                            if (name != null && name.equals(function.getName()) && (
                                prevFunction.isGetProperty() && function.isSetProperty()
                                    || prevFunction.isSetProperty() && function.isGetProperty()
                            )) {
                                PsiElement doc = JSDocumentationUtils.findDocComment(prevFunction);
                                if (doc != null) {
                                    return prevFunction;
                                }
                            }
                        }
                        break;
                    }
                }
            }
            return psiElement;
        }
        else if (object instanceof JSProperty || object instanceof JSStatement || object instanceof JSClass) {
            return (PsiElement)object;
        }
        else if (object instanceof PsiElement psiElement) {
            PsiElement parent = psiElement.getParent();
            if (parent instanceof JSAssignmentExpression assignment) {
                return assignment.getParent();
            }
            else if (parent instanceof JSVarStatement varStatement) {
                if (varStatement.getFirstChild() instanceof JSAttributeList && JSDocumentationUtils.findDocComment(psiElement) != null) {
                    return psiElement;
                }
                return varStatement;
            }
            else if (parent instanceof JSAttribute attribute) {
                PsiElement attrParent = attribute.getParent();
                if (attrParent.getFirstChild() == attribute) {
                    PsiElement attrGrandParent = attrParent.getParent();
                    if (attrGrandParent instanceof JSFile) {
                        return attrParent;
                    }
                    return attrGrandParent;
                }
                return attribute;
            }
            else if (parent instanceof JSSuppressionHolder suppressionHolder) {
                return suppressionHolder;
            }
            else {
                return psiElement;
            }
        }

        return null;
    }

    @Nullable
    @Override
    public PsiElement getDocumentationElementForLink(PsiManager psiManager, String link, PsiElement context) {
        return getDocumentationElementForLinkStatic(psiManager, link, context);
    }

    @Nullable
    private static PsiElement getDocumentationElementForLinkStatic(PsiManager psiManager, String link, @Nonnull PsiElement context) {
        int delimiterIndex = link.lastIndexOf(':');

        String attributeType = null;
        String attributeName = null;
        for (Map.Entry<String, String> e : DOCUMENTED_ATTRIBUTES.entrySet()) {
            String pattern = "." + e.getValue();
            if (link.contains(pattern)) {
                attributeType = e.getKey();
                attributeName = link.substring(link.indexOf(pattern) + pattern.length());
                link = link.substring(0, link.indexOf(pattern));
                break;
            }
        }
        if (delimiterIndex != -1 && attributeType == null) {
            int delimiterIndex2 = link.lastIndexOf(':', delimiterIndex - 1);
            String fileName = link.substring(0, delimiterIndex2).replace(File.separatorChar, '/');
            String name = link.substring(delimiterIndex2 + 1, delimiterIndex);
            int offset = Integer.parseInt(link.substring(delimiterIndex + 1));
            return JavaScriptIndex.findSymbolByFileAndNameAndOffset(fileName, name, offset);
        }
        else if (attributeType != null) {
            PsiElement clazz = JSResolveUtil.findClassByQName(link, context.getResolveScope(), psiManager.getProject());
            if (!(clazz instanceof JSClass)) {
                return null;
            }
            return findNamedAttribute((JSClass)clazz, attributeType, attributeName);
        }
        else {
            PsiElement clazz = JSResolveUtil.findClassByQName(link, context.getResolveScope(), psiManager.getProject());
            if (clazz == null && link.contains(".")) {
                String qname = link.substring(0, link.lastIndexOf('.'));
                clazz = JSResolveUtil.findClassByQName(qname, context.getResolveScope(), psiManager.getProject());
                if (clazz instanceof JSClass jsClass) {
                    String member = link.substring(link.lastIndexOf('.') + 1);

                    if (member.endsWith("()")) {
                        member = member.substring(0, member.length() - 2);

                        PsiElement result = findMethod(jsClass, member);
                        if (result == null) {
                            result = findProperty(jsClass, member); // user might refer to a property
                        }
                        return result;
                    }
                    else {
                        PsiElement result = jsClass.findFieldByName(member);
                        if (result == null) {
                            result = findProperty(jsClass, member);
                        }
                        if (result == null) {
                            result = findMethod(jsClass, member); // user might forget brackets
                        }
                        return result;
                    }
                }
            }

            if (clazz instanceof JSVariable) {
                return clazz;
            }

            if (link.endsWith("()")) {
                link = link.substring(0, link.length() - 2);
                clazz = JSResolveUtil.findClassByQName(link, context.getResolveScope(), psiManager.getProject());
                if (clazz instanceof JSFunction) {
                    return clazz;
                }
            }
            return clazz;
        }
    }

    @Nullable
    @RequiredReadAction
    protected static JSAttributeNameValuePair findNamedAttribute(JSClass clazz, String type, String name) {
        SimpleReference<JSAttributeNameValuePair> attribute = new SimpleReference<>();
        JSResolveUtil.processMetaAttributesForClass(clazz, new JSResolveUtil.MetaDataProcessor() {
            @Override
            @RequiredReadAction
            public boolean process(@Nonnull JSAttribute jsAttribute) {
                if (type.equals(jsAttribute.getName())) {
                    JSAttributeNameValuePair jsAttributeNameValuePair = jsAttribute.getValueByName("name");
                    if (jsAttributeNameValuePair != null && name.equals(jsAttributeNameValuePair.getSimpleValue())) {
                        attribute.set(jsAttributeNameValuePair);
                        return false;
                    }
                }
                return true;
            }

            @Override
            public boolean handleOtherElement(PsiElement el, PsiElement context, @Nullable SimpleReference<PsiElement> continuePassElement) {
                return true;
            }
        });
        return attribute.get();
    }

    private static PsiElement findProperty(JSClass jsClass, String name) {
        PsiElement result = jsClass.findFunctionByNameAndKind(name, JSFunction.FunctionKind.GETTER);
        if (result == null) {
            result = jsClass.findFunctionByNameAndKind(name, JSFunction.FunctionKind.SETTER);
        }
        return result;
    }

    private static PsiElement findMethod(JSClass jsClass, String name) {
        PsiElement result = jsClass.findFunctionByNameAndKind(name, JSFunction.FunctionKind.CONSTRUCTOR);
        if (result == null) {
            result = jsClass.findFunctionByNameAndKind(name, JSFunction.FunctionKind.SIMPLE);
        }
        return result;
    }

    @Nullable
    @Override
    public PsiComment findExistingDocComment(PsiComment contextElement) {
        return contextElement;
    }

    @Nullable
    @Override
    public Pair<PsiElement, PsiComment> parseContext(@Nonnull PsiElement element) {
        return null;
    }

    @Nullable
    @Override
    @RequiredReadAction
    public String generateDocumentationContentStub(PsiComment contextComment) {
        for (PsiElement el = contextComment.getParent(); el != null; el = el.getNextSibling()) {
            if (el instanceof JSProperty property) {
                if (property.getValue() instanceof JSFunction function) {
                    return doGenerateDoc(function);
                }
            }
            else if (el instanceof JSFunction function) {
                return doGenerateDoc(function);
            }
            else if (el instanceof JSExpressionStatement expression) {
                if (expression.getExpression() instanceof JSAssignmentExpression assignment
                    && assignment.getROperand() instanceof JSFunctionExpression functionExpr) {
                    return doGenerateDoc(functionExpr.getFunction());
                }
            }
            else if (el instanceof JSVarStatement varStatement) {
                JSVariable[] variables = varStatement.getVariables();
                if (variables.length > 0 && variables[0].getInitializer() instanceof JSFunctionExpression functionExpr) {
                    return doGenerateDoc(functionExpr.getFunction());
                }
                break;
            }
        }
        return null;
    }

    @RequiredReadAction
    private static String doGenerateDoc(JSFunction function) {
        StringBuilder builder = new StringBuilder();
        JSParameterList parameterList = function.getParameterList();
        PsiFile containingFile = function.getContainingFile();
        boolean ecma = containingFile.getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4;

        if (parameterList != null) {
            for (JSParameter parameter : parameterList.getParameters()) {
                builder.append("* @param ").append(parameter.getName());
                //String s = JSPsiImplUtils.getTypeFromDeclaration(parameter);
                //if (s != null) builder.append(" : ").append(s);
                builder.append("\n");
            }
        }

        if (ecma) {
            String s = JSPsiImplUtils.getTypeFromDeclaration(function);

            if (s != null && !"void".equals(s)) {
                builder.append("* @return ");

                //builder.append(s);
                builder.append("\n");
            }
        }

        return builder.toString();
    }

    @Nullable
    @RequiredReadAction
    protected static String getSeeAlsoLinkResolved(PsiElement originElement, String link) {
        JSQualifiedNamedElement qualifiedElement = findParentQualifiedElement(originElement);
        if (qualifiedElement == null) {
            return null;
        }
        String linkToResolve = getLinkToResolve(qualifiedElement, link);
        PsiElement resolvedElement = getDocumentationElementForLinkStatic(originElement.getManager(), linkToResolve, originElement);
        if (resolvedElement != null) {
            return linkToResolve;
        }
        return null;
    }

    private static String getLinkToResolve(JSQualifiedNamedElement origin, String link) {
        String originQname = origin.getQualifiedName();
        if (link.length() == 0) {
            return originQname;
        }
        else if (StringUtil.startsWithChar(link, '#')) {
            if (origin instanceof JSClass) {
                return originQname + "." + link.substring(1);
            }
            else {
                String aPackage = StringUtil.getPackageName(originQname);
                return aPackage + "." + link.substring(1);
            }
        }
        else {
            String linkFile = link.contains("#") ? link.substring(0, link.lastIndexOf('#')) : link;
            String linkAnchor = link.contains("#") ? link.substring(link.lastIndexOf('#') + 1) : null;

            String qname;
            if (StringUtil.endsWithIgnoreCase(linkFile, HTML_EXTENSION)) {
                String prefix = StringUtil.getPackageName(originQname);
                while (linkFile.startsWith("../")) {
                    linkFile = linkFile.substring("../".length());
                    prefix = StringUtil.getPackageName(prefix);
                }

                String linkFilename;
                if (linkFile.endsWith(PACKAGE_FILE)) {
                    linkFilename = StringUtil.trimEnd(linkFile, PACKAGE_FILE);
                    linkFilename = StringUtil.trimEnd(linkFilename, "/");
                }
                else {
                    linkFilename = linkFile.substring(0, linkFile.lastIndexOf("."));
                }
                if (linkFilename.length() > 0) {
                    qname = (prefix.length() > 0 ? prefix + "." : prefix) + linkFilename.replaceAll("/", ".");
                }
                else {
                    qname = prefix;
                }
            }
            else {
                qname = linkFile;
            }

            return linkAnchor != null ? (qname.length() > 0 ? qname + "." : qname) + linkAnchor : qname;
        }
    }

    @Nullable
    @RequiredReadAction
    protected static JSQualifiedNamedElement findParentQualifiedElement(PsiElement element) {
        if (element instanceof JSClass jsClass) {
            return jsClass;
        }
        if (element instanceof JSFunction || element instanceof JSVariable) {
            PsiElement parent = JSResolveUtil.findParent(element);
            if (parent instanceof JSClass jsClass) {
                return jsClass;
            }
            else if (parent instanceof JSFile) {
                return (JSQualifiedNamedElement) element;
            }
        }

        JSAttribute attribute = null;
        if (element instanceof JSAttribute jsAttribute) {
            attribute = jsAttribute;
        }
        else if (element instanceof JSAttributeNameValuePair nameValuePair) {
            attribute = (JSAttribute)nameValuePair.getParent();
        }

        if (attribute != null && DOCUMENTED_ATTRIBUTES.containsKey(attribute.getName())) {
            JSClass jsClass = PsiTreeUtil.getParentOfType(element, JSClass.class);
            if (jsClass != null) {
                return jsClass;
            }
        }

        return null;
    }

    @Nonnull
    @Override
    public Language getLanguage() {
        return JavaScriptLanguage.INSTANCE;
    }
}

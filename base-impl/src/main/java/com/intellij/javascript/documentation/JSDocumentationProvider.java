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
import consulo.util.lang.ref.Ref;
import consulo.xml.psi.xml.XmlToken;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import org.jetbrains.annotations.NonNls;

import java.io.File;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * User: Maxim.Mossienko
 * Date: Nov 4, 2005
 * Time: 5:04:28 PM
 */
@ExtensionImpl
public class JSDocumentationProvider implements CodeDocumentationProvider, LanguageDocumentationProvider {
    private DocumentationProvider cssProvider;
    @NonNls
    private static final String OBJECT_NAME = "Object";
    protected static final String SEE_PLAIN_TEXT_CHARS = "\t \"-\\/<>*";

    @NonNls
    protected static final String PACKAGE = "package";
    @NonNls
    protected static final String HTML_EXTENSION = ".html";
    @NonNls
    protected static final String PACKAGE_FILE = PACKAGE + HTML_EXTENSION;

    protected static final Map<String, String> DOCUMENTED_ATTRIBUTES;

    static {
        DOCUMENTED_ATTRIBUTES = new HashMap<String, String>();
        DOCUMENTED_ATTRIBUTES.put("Event", "event:");
        DOCUMENTED_ATTRIBUTES.put("Style", "style:");
        DOCUMENTED_ATTRIBUTES.put("Effect", "effect:");
    }

    private DocumentationProvider getCssProvider(Project project) throws Exception {
        if (cssProvider == null) {
            final Class<?> aClass = Class.forName("com.intellij.psi.css.impl.util.CssDocumentationProvider");
            cssProvider = (DocumentationProvider)aClass.getConstructor(new Class[]{}).newInstance(new Object[]{});
        }

        return cssProvider;
    }

    @Nullable
    public String getQuickNavigateInfo(PsiElement element, PsiElement element2) {
        if (element instanceof JSFunction) {
            final JSFunction function = (JSFunction)element;
            final PsiElement parent = element.getParent();

            if (function.isConstructor() && parent instanceof JSClass jsClass) {
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
        else if (element instanceof JSAttributeNameValuePair) {
            return createQuickNavigateForAnnotationDerived(element);
        }
        else if (element instanceof XmlToken) {
            BaseJSSymbolProcessor.TagContextBuilder builder = new BaseJSSymbolProcessor.TagContextBuilder(element, "XmlTag");
            return StringUtil.stripQuotesAroundValue(element.getText()) + ":" + builder.typeName;
        }
        else if (element instanceof JSNamespaceDeclaration namespaceDeclaration) {
            return createQuickNavigateForNamespace(namespaceDeclaration);
        }

        return null;
    }

    private static String createQuickNavigateForAnnotationDerived(final PsiElement element) {
        final JSAttributeNameValuePair valuePair = (JSAttributeNameValuePair)element;
        final JSAttribute parent = (JSAttribute)valuePair.getParent();
        final StringBuilder builder = new StringBuilder();
        final JSClass clazz = PsiTreeUtil.getParentOfType(valuePair, JSClass.class);
        appendParentInfo(clazz != null ? clazz : parent.getContainingFile(), builder, parent);
        builder.append(parent.getName()).append(" ").append(valuePair.getSimpleValue());
        return builder.toString();
    }

    @Nullable
    private static String createQuickNavigateForFunction(final JSFunction function) {
        final PsiElement parent = JSResolveUtil.findParent(function);
        final StringBuilder result = new StringBuilder();

        appendParentInfo(parent, result, function);

        appendAttrList(function, result);
        final boolean get = function.isGetProperty();
        final boolean set = function.isSetProperty();

        result.append(get || set ? "property " : "function ");
        result.append(function.getName());

        if (!get && !set) {
            result.append('(');
            final JSParameterList jsParameterList = function.getParameterList();

            if (jsParameterList != null) {
                final int start = result.length();

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
            final JSParameterList jsParameterList = function.getParameterList();

            if (jsParameterList != null) {
                final JSParameter[] jsParameters = jsParameterList.getParameters();
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

    private static void appendParentInfo(final PsiElement parent, final StringBuilder builder, PsiNamedElement element) {
        if (parent instanceof JSClass) {
            builder.append(((JSClass)parent).getQualifiedName()).append("\n");
        }
        else if (parent instanceof JSPackageStatement) {
            builder.append(((JSPackageStatement)parent).getQualifiedName()).append("\n");
        }
        else if (parent instanceof JSFile) {
            if (parent.getContext() != null) {
                final String mxmlPackage = JSResolveUtil.findPackageForMxml(parent);
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
                        final String s = node.getText();
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
    private static String createQuickNavigateForVariable(final JSVariable variable) {
        final PsiElement parent = JSResolveUtil.findParent(variable);
        final StringBuilder result = new StringBuilder();

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

    private static void appendVarType(final JSVariable variable, final StringBuilder builder) {
        final String varType = variable.getTypeString();
        if (varType != null) {
            builder.append(':').append(varType);
        }
    }

    private static
    @Nullable
    String createQuickNavigateForClazz(final JSClass jsClass) {
        final String qName = jsClass.getQualifiedName();
        if (qName == null) {
            return null;
        }
        StringBuilder result = new StringBuilder();
        String packageName = StringUtil.getPackageName(qName);
        if (packageName.length() > 0) {
            result.append(packageName).append("\n");
        }

        appendAttrList(jsClass, result);
        if (jsClass.isInterface()) {
            result.append("interface");
        }
        else {
            result.append("class");
        }

        final String name = jsClass.getName();
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

    private static
    @Nullable
    String createQuickNavigateForNamespace(final JSNamespaceDeclaration ns) {
        final String qName = ns.getQualifiedName();
        if (qName == null) {
            return null;
        }
        StringBuilder result = new StringBuilder();
        String packageName = StringUtil.getPackageName(qName);
        if (packageName.length() > 0) {
            result.append(packageName).append("\n");
        }

        result.append("namespace");

        final String name = ns.getName();
        result.append(" ").append(name);

        String s = ns.getInitialValueString();
        if (s != null) {
            result.append(" = ").append(s);
        }
        return result.toString();
    }


    private static void appendAttrList(final JSAttributeListOwner jsClass, final StringBuilder result) {
        final JSAttributeList attributeList = jsClass.getAttributeList();
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

    private static
    @Nullable
    String generateReferenceTargetList(final @Nullable JSReferenceList implementsList, @Nonnull String packageName) {
        if (implementsList == null) {
            return null;
        }
        StringBuilder result = null;

        final String[] referenceExpressionTexts = implementsList.getReferenceTexts();

        for (String refExprText : referenceExpressionTexts) {
            refExprText = JSImportHandlingUtil.resolveTypeName(refExprText, implementsList);
            if (result == null) {
                result = new StringBuilder();
            }
            else {
                result.append(",");
            }

            final String referencedPackageName = StringUtil.getPackageName(refExprText);
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
                final DocumentationProvider documentationProvider = getCssProvider(element.getProject());
                final Method method = documentationProvider.getClass().getMethod("getUrlFor", new Class[]{String.class});
                final Object o = method.invoke(null, new Object[]{possibleCssName});

                if (o instanceof String) {
                    return Collections.singletonList((String)o);
                }
            }
            catch (Exception e) {
            }
        }
        return null;
    }

    @Override
    public String generateDoc(PsiElement _element, PsiElement originalElement) {
        if (_element instanceof JSReferenceExpression) {
            StringBuilder buffer = null;

            // ambigious reference
            final JSReferenceExpression expression = (JSReferenceExpression)_element;
            for (ResolveResult r : expression.multiResolve(false)) {
                if (buffer == null) {
                    buffer = new StringBuilder();
                }
                final PsiElement element = r.getElement();
                final ItemPresentation presentation = ((NavigationItem)element).getPresentation();

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

        final boolean parameterDoc = element instanceof JSParameter;
        if (parameterDoc) {
            element = findElementForWhichPreviousCommentWillBeSearched(PsiTreeUtil.getParentOfType(element, JSFunction.class));
        }

        if (element != null) {
            PsiElement docComment =
                JSDocumentationUtils.findDocComment(element, _element instanceof JSAttributeNameValuePair ? originalElement : null);

            if (docComment != null) {
                docComment = findFirstDocComment(docComment);
                element = findTargetElement(_element, element);
                final JSDocumentationBuilder builder = new JSDocumentationBuilder(element, originalElement);
                JSDocumentationUtils.processDocumentationTextFromComment(docComment.getNode(), builder);

                return parameterDoc ? builder.getParameterDoc(((JSParameter)_element).getName()) : builder.getDoc();
            }

            element = findTargetElement(_element, element);

            if (element instanceof JSFunction) {
                ASTNode initialComment = JSDocumentationUtils.findLeadingCommentInFunctionBody(element);

                if (initialComment != null) {
                    final JSDocumentationBuilder builder = new JSDocumentationBuilder(element, originalElement);
                    JSDocumentationUtils.processDocumentationTextFromComment(initialComment, builder);
                    return builder.getDoc();
                }
            }
        }

        String possibleCssName = findPossibleCssName(_element);
        if (possibleCssName != null) {
            try {
                final DocumentationProvider documentationProvider = getCssProvider(_element.getProject());
                final Method declaredMethod =
                    documentationProvider.getClass().getDeclaredMethod("generateDoc", String.class, PsiElement.class);
                final Object o = declaredMethod.invoke(null, possibleCssName, null);

                if (o instanceof String) {
                    return (String)o;
                }
            }
            catch (Exception e) {
            }
        }

        return null;
    }

    private static PsiElement findTargetElement(final PsiElement _element, PsiElement element) {
        if (_element instanceof JSDefinitionExpression) {
            final PsiElement parentElement = _element.getParent();

            if (parentElement instanceof JSAssignmentExpression assignment) {
                final JSExpression rOperand = assignment.getROperand();
                element = rOperand instanceof JSFunctionExpression ? rOperand : _element;
            }
        }
        else if (_element instanceof JSFunctionExpression) {
            element = _element;
        }
        else if (_element instanceof JSProperty property) {
            final JSExpression expression = property.getValue();

            if (expression instanceof JSFunction) {
                element = expression;
            }
        }
        else if (_element instanceof JSVariable) {
            if (_element instanceof JSParameter) {
                return PsiTreeUtil.getParentOfType(_element, JSFunction.class);
            }
            element = _element;
        }
        else if (_element instanceof JSAttributeNameValuePair) {
            return _element;
        }
        return element;
    }

    private static PsiElement findFirstDocComment(PsiElement docComment) {
        if (docComment.getNode().getElementType() == JSTokenTypes.END_OF_LINE_COMMENT) {
            while (true) {
                PsiElement prev = docComment.getPrevSibling();
                if (prev instanceof PsiWhiteSpace) {
                    prev = prev.getPrevSibling();
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
        if (_element instanceof JSDefinitionExpression definitionExpression) {
            final JSExpression expression = definitionExpression.getExpression();

            if (expression instanceof JSReferenceExpression referenceExpression) {
                final String text = referenceExpression.getReferencedName();
                if (text == null) {
                    return null;
                }
                final StringBuffer buf = new StringBuffer(text.length());

                for (int i = 0; i < text.length(); ++i) {
                    final char ch = text.charAt(i);

                    if (Character.isUpperCase(ch)) {
                        buf.append('-').append(Character.toLowerCase(ch));
                    }
                    else {
                        buf.append(ch);
                    }
                }

                return buf.toString();
            }
        }
        return null;
    }

    @Override
    public PsiElement getDocumentationElementForLookupItem(PsiManager psiManager, Object object, PsiElement element) {
        final PsiElement psiElement = findElementForWhichPreviousCommentWillBeSearched(object);
        if (psiElement != null && JSDocumentationUtils.findDocComment(psiElement) != null) {
            return psiElement;
        }
        if (object instanceof PsiElement) {
            return (PsiElement)object;
        }
        return null;
    }

    public static PsiElement findElementForWhichPreviousCommentWillBeSearched(Object object) {
        if (object instanceof JSFunction function) {
            PsiElement psiElement = function;
            PsiElement parent = psiElement.getParent();
            if (parent instanceof JSNewExpression) {
                parent = parent.getParent();
            }
            if (parent instanceof JSProperty) {
                psiElement = parent;
            }
            else if (parent instanceof JSAssignmentExpression) {
                psiElement = parent.getParent();
            }

            if (function.isSetProperty() || function.isGetProperty()) {
                for (PsiElement el = function.getPrevSibling(); el != null; el = el.getPrevSibling()) {
                    if (!(el instanceof PsiWhiteSpace) && !(el instanceof PsiComment)) {
                        if (el instanceof JSFunction prevFunction) {
                            String name = prevFunction.getName();

                            if (name != null && name.equals(function.getName()) && (
                                (prevFunction.isGetProperty() && function.isSetProperty())
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
            final PsiElement parent = psiElement.getParent();
            if (parent instanceof JSAssignmentExpression) {
                return parent.getParent();
            }
            else if (parent instanceof JSVarStatement) {
                final PsiElement firstChild = parent.getFirstChild();
                if (firstChild instanceof JSAttributeList) {
                    if (JSDocumentationUtils.findDocComment((PsiElement)object) != null) {
                        return (PsiElement)object;
                    }
                }
                return parent;
            }
            else if (parent instanceof JSAttribute) {
                final PsiElement grandParent = parent.getParent();
                if (grandParent.getFirstChild() == parent) {
                    final PsiElement element = grandParent.getParent();
                    if (element instanceof JSFile) {
                        return grandParent;
                    }
                    return element;
                }
                return parent;
            }
            else if (parent instanceof JSSuppressionHolder) {
                return parent;
            }
            else {
                return (PsiElement)object;
            }
        }

        return null;
    }

    @Override
    @Nullable
    public PsiElement getDocumentationElementForLink(final PsiManager psiManager, String link, final PsiElement context) {
        return getDocumentationElementForLinkStatic(psiManager, link, context);
    }

    @Nullable
    private static PsiElement getDocumentationElementForLinkStatic(final PsiManager psiManager, String link, @Nonnull PsiElement context) {
        final int delimiterIndex = link.lastIndexOf(':');

        String attributeType = null;
        String attributeName = null;
        for (Map.Entry<String, String> e : DOCUMENTED_ATTRIBUTES.entrySet()) {
            final String pattern = "." + e.getValue();
            if (link.contains(pattern)) {
                attributeType = e.getKey();
                attributeName = link.substring(link.indexOf(pattern) + pattern.length());
                link = link.substring(0, link.indexOf(pattern));
                break;
            }
        }
        if (delimiterIndex != -1 && attributeType == null) {
            final int delimiterIndex2 = link.lastIndexOf(':', delimiterIndex - 1);
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
    protected static JSAttributeNameValuePair findNamedAttribute(JSClass clazz, final String type, final String name) {
        final Ref<JSAttributeNameValuePair> attribute = new Ref<JSAttributeNameValuePair>();
        JSResolveUtil.processMetaAttributesForClass(clazz, new JSResolveUtil.MetaDataProcessor() {
            @Override
            public boolean process(@Nonnull JSAttribute jsAttribute) {
                if (type.equals(jsAttribute.getName())) {
                    final JSAttributeNameValuePair jsAttributeNameValuePair = jsAttribute.getValueByName("name");
                    if (jsAttributeNameValuePair != null && name.equals(jsAttributeNameValuePair.getSimpleValue())) {
                        attribute.set(jsAttributeNameValuePair);
                        return false;
                    }
                }
                return true;
            }

            @Override
            public boolean handleOtherElement(PsiElement el, PsiElement context, @Nullable Ref<PsiElement> continuePassElement) {
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

    @Override
    @Nullable
    public PsiComment findExistingDocComment(PsiComment contextElement) {
        return contextElement;
    }

    @Nullable
    @Override
    public Pair<PsiElement, PsiComment> parseContext(@Nonnull PsiElement element) {
        return null;
    }

    @Override
    @Nullable
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
            else if (el instanceof JSExpressionStatement expressionStatement) {
                if (expressionStatement.getExpression() instanceof JSAssignmentExpression assignmentExpression
                    && assignmentExpression.getROperand() instanceof JSFunctionExpression functionExpression) {
                    return doGenerateDoc(functionExpression.getFunction());
                }
            }
            else if (el instanceof JSVarStatement varStatement) {
                JSVariable[] variables = varStatement.getVariables();
                if (variables.length > 0 && variables[0].getInitializer() instanceof JSFunctionExpression functionExpression) {
                    return doGenerateDoc(functionExpression.getFunction());
                }
                break;
            }
        }
        return null;
    }

    private static String doGenerateDoc(final JSFunction function) {
        StringBuilder builder = new StringBuilder();
        final JSParameterList parameterList = function.getParameterList();
        final PsiFile containingFile = function.getContainingFile();
        final boolean ecma = containingFile.getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4;

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
    protected static String getSeeAlsoLinkResolved(PsiElement originElement, String link) {
        JSQualifiedNamedElement qualifiedElement = findParentQualifiedElement(originElement);
        if (qualifiedElement == null) {
            return null;
        }
        String linkToResolve = getLinkToResolve(qualifiedElement, link);
        final PsiElement resolvedElement = getDocumentationElementForLinkStatic(originElement.getManager(), linkToResolve, originElement);
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

            final String qname;
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
    protected static JSQualifiedNamedElement findParentQualifiedElement(PsiElement element) {
        if (element instanceof JSClass jsClass) {
            return jsClass;
        }
        if (element instanceof JSFunction || element instanceof JSVariable) {
            final PsiElement parent = JSResolveUtil.findParent(element);
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
        else if (element instanceof JSAttributeNameValuePair) {
            attribute = (JSAttribute)element.getParent();
        }

        if (attribute != null && DOCUMENTED_ATTRIBUTES.containsKey(attribute.getName())) {
            final JSClass jsClass = PsiTreeUtil.getParentOfType(element, JSClass.class);
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

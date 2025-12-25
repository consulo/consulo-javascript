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

/*
 * Created by IntelliJ IDEA.
 * User: Maxim.Mossienko
 * Date: Nov 15, 2006
 * Time: 4:48:46 PM
 */
package com.intellij.javascript.documentation;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.ast.TokenSet;
import consulo.language.ast.TokenType;
import consulo.language.editor.documentation.DocumentationManagerUtil;
import consulo.language.psi.*;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.util.lang.StringUtil;
import consulo.util.lang.ref.SimpleReference;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class JSDocumentationUtils {
    private static final Pattern DOJO_PARAMETERS_PATTERN = Pattern.compile("^\\s*(\\w+):(.*)$");
    private static final Pattern JS_DOC_PARAMETERS_PATTERN = Pattern.compile(
        "^\\s*@param\\s*(?:\\{([^}]+)}\\s*)?(\\w+)?(?:\\s:\\s(\\S+))?(?:\\s*\\[(\\w+)(?:\\.(\\w+))?(?:=([^\\]]*))?])?(.*)$"
    );
    private static final Pattern JS_DOC_EVENT_PATTERN = Pattern.compile("^\\s*@event\\s*(\\w+)(?:\\s:\\s(\\S+))?(.*)$");
    private static final Pattern JS_DOC_REMARK_PATTERN = Pattern.compile("^\\s*@remarks (.*)$");
    private static final Pattern JS_DOC_METHOD_PATTERN = Pattern.compile("^\\s*@method\\s*(\\w+)(.*)$");
    private static final Pattern JS_DOC_CLASS_PATTERN = Pattern.compile("^\\s*@class\\s*(\\w+(?:\\.\\w+)*)(.*)$");
    private static final Pattern JS_DOC_DEPRECATED_PATTERN = Pattern.compile("^\\s*@deprecated\\s*(.*)$");
    private static final Pattern JS_DOC_CONSTRUCTOR_PATTERN = Pattern.compile("^\\s*@constructor$");
    private static final Pattern JS_DOC_FINAL_PATTERN = Pattern.compile("^\\s*@final$");
    private static final Pattern JS_DOC_PRIVATE_PATTERN = Pattern.compile("^\\s*@private$");
    private static final Pattern JS_DOC_PUBLIC_PATTERN = Pattern.compile("^\\s*@public$");
    private static final Pattern JS_DOC_PROTECTED_PATTERN = Pattern.compile("^\\s*@protected$");
    private static final Pattern JS_DOC_OPTIONAL_PATTERN = Pattern.compile("^\\s*@optional(.*)$");
    private static final Pattern JS_DOC_STATIC_PATTERN = Pattern.compile("^\\s*@static$");
    private static final Pattern JS_DOC_SEE_PATTERN = Pattern.compile("^\\s*@see (.*)$");
    private static final Pattern JS_DOC_DESCRIPTION_PATTERN = Pattern.compile("^\\s*@description\\s*(.+)$");
    private static final Pattern JS_DOC_RETURN_PATTERN =
        Pattern.compile("^\\s*@return(?:s)?\\s*(?:(?:\\{|:)?\\s*([^\\s\\}]+)\\s*\\}?\\s*)?(.*)$");
    private static final Pattern JS_DOC_NAMESPACE_PATTERN = Pattern.compile("^\\s*@namespace\\s*([\\w\\.]+)(.*)$");
    private static final Pattern JS_DOC_PROPERTY_PATTERN = Pattern.compile("^\\s*@property\\s*(\\w+)(.*)$");
    private static final Pattern JS_DOC_TYPE_PATTERN = Pattern.compile("^\\s*@type\\s*\\{?([^\\s\\}]+)\\}?(.*)$");
    private static final Pattern JS_DOC_REQUIRES_PATTERN = Pattern.compile("^\\s*@requires\\s*(\\S+)(.*)$");
    private static final Pattern JS_DOC_DEFAULT_PATTERN = Pattern.compile("^\\s*@default\\s*(.*)$");
    private static final Pattern JS_DOC_EXTENDS_PATTERN = Pattern.compile("^\\s*@extends\\s*(.*)$");
    private static final Map<Pattern, String> PATTERN_TO_HINT_MAP = new HashMap<>();
    private static final Map<Pattern, JSDocumentationProcessor.MetaDocType> PATTERN_TO_META_DOC_TYPE_MAP = new HashMap<>();

    static {
        PATTERN_TO_HINT_MAP.put(DOJO_PARAMETERS_PATTERN, ":");
        PATTERN_TO_META_DOC_TYPE_MAP.put(DOJO_PARAMETERS_PATTERN, JSDocumentationProcessor.MetaDocType.PARAMETER);
        PATTERN_TO_HINT_MAP.put(JS_DOC_PARAMETERS_PATTERN, "@pa");
        PATTERN_TO_META_DOC_TYPE_MAP.put(JS_DOC_PARAMETERS_PATTERN, JSDocumentationProcessor.MetaDocType.PARAMETER);

        PATTERN_TO_HINT_MAP.put(JS_DOC_METHOD_PATTERN, "@m");
        PATTERN_TO_META_DOC_TYPE_MAP.put(JS_DOC_METHOD_PATTERN, JSDocumentationProcessor.MetaDocType.METHOD);

        PATTERN_TO_HINT_MAP.put(JS_DOC_OPTIONAL_PATTERN, "@o");
        PATTERN_TO_META_DOC_TYPE_MAP.put(JS_DOC_OPTIONAL_PATTERN, JSDocumentationProcessor.MetaDocType.OPTIONAL_PARAMETERS);

        PATTERN_TO_HINT_MAP.put(JS_DOC_EVENT_PATTERN, "@ev");
        PATTERN_TO_META_DOC_TYPE_MAP.put(JS_DOC_EVENT_PATTERN, JSDocumentationProcessor.MetaDocType.EVENT);

        PATTERN_TO_HINT_MAP.put(JS_DOC_EXTENDS_PATTERN, "@ex");
        PATTERN_TO_META_DOC_TYPE_MAP.put(JS_DOC_EXTENDS_PATTERN, JSDocumentationProcessor.MetaDocType.EXTENDS);

        PATTERN_TO_HINT_MAP.put(JS_DOC_REMARK_PATTERN, "@rem");
        PATTERN_TO_META_DOC_TYPE_MAP.put(JS_DOC_REMARK_PATTERN, JSDocumentationProcessor.MetaDocType.NOTE);

        PATTERN_TO_HINT_MAP.put(JS_DOC_RETURN_PATTERN, "@ret");
        PATTERN_TO_META_DOC_TYPE_MAP.put(JS_DOC_RETURN_PATTERN, JSDocumentationProcessor.MetaDocType.RETURN);

        PATTERN_TO_HINT_MAP.put(JS_DOC_PUBLIC_PATTERN, "@pu");
        PATTERN_TO_META_DOC_TYPE_MAP.put(JS_DOC_PUBLIC_PATTERN, JSDocumentationProcessor.MetaDocType.PUBLIC);

        PATTERN_TO_HINT_MAP.put(JS_DOC_PROTECTED_PATTERN, "@prot");
        PATTERN_TO_META_DOC_TYPE_MAP.put(JS_DOC_PROTECTED_PATTERN, JSDocumentationProcessor.MetaDocType.PROTECTED);

        PATTERN_TO_HINT_MAP.put(JS_DOC_STATIC_PATTERN, "@st");
        PATTERN_TO_META_DOC_TYPE_MAP.put(JS_DOC_STATIC_PATTERN, JSDocumentationProcessor.MetaDocType.STATIC);

        PATTERN_TO_HINT_MAP.put(JS_DOC_SEE_PATTERN, "@se");
        PATTERN_TO_META_DOC_TYPE_MAP.put(JS_DOC_SEE_PATTERN, JSDocumentationProcessor.MetaDocType.SEE);

        PATTERN_TO_HINT_MAP.put(JS_DOC_DESCRIPTION_PATTERN, "@des");
        PATTERN_TO_META_DOC_TYPE_MAP.put(JS_DOC_DESCRIPTION_PATTERN, JSDocumentationProcessor.MetaDocType.DESCRIPTION);

        PATTERN_TO_HINT_MAP.put(JS_DOC_DEPRECATED_PATTERN, "@dep");
        PATTERN_TO_META_DOC_TYPE_MAP.put(JS_DOC_DEPRECATED_PATTERN, JSDocumentationProcessor.MetaDocType.DEPRECATED);

        PATTERN_TO_HINT_MAP.put(JS_DOC_CONSTRUCTOR_PATTERN, "@co");

        PATTERN_TO_META_DOC_TYPE_MAP.put(JS_DOC_CONSTRUCTOR_PATTERN, JSDocumentationProcessor.MetaDocType.CONSTRUCTOR);

        PATTERN_TO_HINT_MAP.put(JS_DOC_CLASS_PATTERN, "@cl");
        PATTERN_TO_META_DOC_TYPE_MAP.put(JS_DOC_CLASS_PATTERN, JSDocumentationProcessor.MetaDocType.CLASS);
        PATTERN_TO_HINT_MAP.put(JS_DOC_PRIVATE_PATTERN, "@pri");
        PATTERN_TO_META_DOC_TYPE_MAP.put(JS_DOC_PRIVATE_PATTERN, JSDocumentationProcessor.MetaDocType.PRIVATE);

        PATTERN_TO_HINT_MAP.put(JS_DOC_NAMESPACE_PATTERN, "@n");
        PATTERN_TO_META_DOC_TYPE_MAP.put(JS_DOC_NAMESPACE_PATTERN, JSDocumentationProcessor.MetaDocType.NAMESPACE);

        PATTERN_TO_HINT_MAP.put(JS_DOC_PROPERTY_PATTERN, "@prop");
        PATTERN_TO_HINT_MAP.put(JS_DOC_TYPE_PATTERN, "@t");
        PATTERN_TO_META_DOC_TYPE_MAP.put(JS_DOC_TYPE_PATTERN, JSDocumentationProcessor.MetaDocType.TYPE);

        PATTERN_TO_HINT_MAP.put(JS_DOC_FINAL_PATTERN, "@f");
        PATTERN_TO_META_DOC_TYPE_MAP.put(JS_DOC_FINAL_PATTERN, JSDocumentationProcessor.MetaDocType.FINAL);
        PATTERN_TO_HINT_MAP.put(JS_DOC_REQUIRES_PATTERN, "@req");
        PATTERN_TO_META_DOC_TYPE_MAP.put(JS_DOC_REQUIRES_PATTERN, JSDocumentationProcessor.MetaDocType.REQUIRES);

        PATTERN_TO_HINT_MAP.put(JS_DOC_DEFAULT_PATTERN, "@def");
        PATTERN_TO_META_DOC_TYPE_MAP.put(JS_DOC_DEFAULT_PATTERN, JSDocumentationProcessor.MetaDocType.DEFAULT);
    }

    private static final Map<Pattern, String> prefixToPatternToHintMap = new HashMap<>();

    static {
        prefixToPatternToHintMap.put(Pattern.compile("^\\s*description:(.*)$"), "descr");

        prefixToPatternToHintMap.put(Pattern.compile("^ summary(?::)?(.*)$"), "summ");

        prefixToPatternToHintMap.put(Pattern.compile("^\\s*\\*(?:\\*)?(.*)$"), "*");

        prefixToPatternToHintMap.put(Pattern.compile("^[/]+(.*)$"), "/");

        prefixToPatternToHintMap.put(Pattern.compile("^\\s*Parameters:(.*)$"), "Parame");
    }

    public static void processDocumentationTextFromComment(ASTNode _initialComment, JSDocumentationProcessor processor) {
        ASTNode prev = _initialComment.getTreePrev();

        if (prev != null && prev.getPsi() instanceof OuterLanguageElement) {
            while (prev.getPsi() instanceof OuterLanguageElement) {
                prev = prev.getTreePrev();
            }
        }
        else {
            prev = null;
        }

        if (prev != null && !(prev.getPsi() instanceof PsiComment)) {
            prev = null;
        }

        final ASTNode initialComment = prev != null ? prev : _initialComment;
        Enumeration<Object> commentLineIterator;

        if (initialComment.getElementType() == JSTokenTypes.END_OF_LINE_COMMENT) {
            commentLineIterator = new Enumeration<>() {
                ASTNode commentNode = initialComment;

                @Override
                public boolean hasMoreElements() {
                    return commentNode != null;
                }

                @Override
                public String nextElement() {
                    ASTNode resultCommentNode = commentNode;
                    commentNode = commentNode.getTreeNext();

                    if (commentNode != null && commentNode.getElementType() == TokenType.WHITE_SPACE) {
                        commentNode = commentNode.getTreeNext();
                    }

                    if (commentNode != null && commentNode.getElementType() != JSTokenTypes.END_OF_LINE_COMMENT) {
                        commentNode = null;
                    }

                    String text = resultCommentNode.getText();
                    if (text.startsWith("//")) {
                        return text.substring(2);
                    }
                    return "";
                }
            };
        }
        else {
            String text = initialComment.getText();
            text = unwrapCommentDelimiters(text);

            commentLineIterator = new StringTokenizer(text, "\r\n");
        }

        boolean needPlainCharData = processor.needsPlainCommentData();

        while (commentLineIterator.hasMoreElements()) {
            String s = (String)commentLineIterator.nextElement();
            if (s.indexOf('@') == -1 && s.indexOf(':') == -1 && !needPlainCharData) {
                continue;
            }

            String commentText = s.replace('\t', ' ');

            for (Map.Entry<Pattern, String> entry : prefixToPatternToHintMap.entrySet()) {
                Matcher matcher = commentText.contains(entry.getValue()) ? entry.getKey().matcher(commentText) : null;
                if (matcher == null) {
                    continue;
                }

                if (matcher.matches()) {
                    commentText = matcher.group(1);
                    break;
                }
            }

            boolean matchedSomething = false;

            for (Map.Entry<Pattern, String> entry : PATTERN_TO_HINT_MAP.entrySet()) {
                Matcher matcher = commentText.contains(entry.getValue()) ? entry.getKey().matcher(commentText) : null;
                if (matcher == null) {
                    continue;
                }

                if (matcher.matches()) {
                    JSDocumentationProcessor.MetaDocType docType = PATTERN_TO_META_DOC_TYPE_MAP.get(entry.getKey());
                    if (docType != null) {
                        int groupCount = matcher.groupCount();
                        String remainingLineContent = groupCount > 0 ? matcher.group(groupCount) : null;
                        String matchName = groupCount > 1 ? matcher.group(1) : null;
                        String matchValue = groupCount > 2 ? matcher.group(2) : null;

                        boolean reportAboutOptionalParameter = false;
                        boolean reportAboutFieldInParameter = false;
                        boolean reportAboutDefaultValue = false;

                        int groupForInitialValue = 6;
                        int groupForFieldName = 5;
                        String fieldName = null;

                        if (groupCount == 7 && entry.getKey() == JS_DOC_PARAMETERS_PATTERN) {
                            String paramNameInBracket = matcher.group(4);

                            if (paramNameInBracket != null) {
                                String tmp = matchName;
                                matchName = paramNameInBracket;
                                matchValue = tmp;
                                reportAboutFieldInParameter = (fieldName = matcher.group(groupForFieldName)) != null;
                                reportAboutOptionalParameter = true;
                                reportAboutDefaultValue = matcher.group(groupForInitialValue) != null;
                            }
                            else {
                                String typeAfterParamName = matcher.group(3);
                                if (typeAfterParamName != null) {
                                    matchName = matchValue;
                                    matchValue = typeAfterParamName;
                                }
                                else {
                                    String tmp = matchValue;
                                    matchValue = matchName;
                                    matchName = tmp;
                                }
                            }
                        }

                        String matched = entry.getKey().pattern();

                        if (reportAboutFieldInParameter) {
                            if (!processor.onPatternMatch(
                                JSDocumentationProcessor.MetaDocType.FIELD,
                                matchName,
                                null,
                                matcher.group(groupForFieldName),
                                commentText,
                                matched
                            )) {
                                break;
                            }
                        }
                        else if (!processor.onPatternMatch(docType, matchName, matchValue, remainingLineContent, commentText, matched)) {
                            break;
                        }
                        if (reportAboutOptionalParameter) {
                            if (!processor.onPatternMatch(
                                JSDocumentationProcessor.MetaDocType.OPTIONAL_PARAMETERS,
                                matchName,
                                fieldName,
                                null,
                                commentText,
                                matched
                            )) {
                                break;
                            }
                        }

                        if (reportAboutDefaultValue) {
                            if (!processor.onPatternMatch(
                                JSDocumentationProcessor.MetaDocType.DEFAULT,
                                matchName,
                                fieldName,
                                matcher.group(groupForInitialValue),
                                commentText,
                                matched
                            )) {
                                break;
                            }
                        }
                    }
                    matchedSomething = true;
                    break;
                }
            }

            if (!matchedSomething && needPlainCharData && !processor.onCommentLine(commentText)) {
                break;
            }
        }
    }

    public static String unwrapCommentDelimiters(String text) {
        if (text.startsWith("/**")) {
            text = text.substring(3);
        }
        else if (text.startsWith("/*") || text.startsWith("//")) {
            text = text.substring(2);
        }

        if (text.endsWith("*/")) {
            text = text.substring(0, text.length() - 2);
        }
        return text;
    }

    @Nullable
    static ASTNode findTrailingCommentInFunctionBody(@Nonnull JSFunction function) {
        ASTNode block = function.getNode().findChildByType(JSElementTypes.BLOCK_STATEMENT);
        if (block == null) {
            return null;
        }

        for (ASTNode prev = block.getLastChildNode(); prev != null; prev = prev.getTreePrev()) {
            if (prev.getElementType() == JSElementTypes.RETURN_STATEMENT) {
                return block.findChildByType(JSTokenTypes.COMMENTS, prev);
            }
            else if (prev.getPsi() instanceof JSStatement) {
                break;
            }
        }
        return null;
    }

    @Nullable
    static ASTNode findLeadingCommentInFunctionBody(@Nonnull PsiElement element) {
        ASTNode functionNode = element.getNode();
        ASTNode block = functionNode.findChildByType(JSElementTypes.BLOCK_STATEMENT);
        if (block == null) {
            return null;
        }

        for (ASTNode node = block.getFirstChildNode().getTreeNext(); node != null; node = node.getTreeNext()) {
            IElementType nodeType = node.getElementType();

            if (nodeType != TokenType.WHITE_SPACE) {
                if (JSTokenTypes.COMMENTS.contains(nodeType)) {
                    return node;
                }
                return null;
            }
        }

        return null;
    }

    @RequiredReadAction
    public static PsiElement findDocComment(PsiElement element) {
        return findDocComment(element, null);
    }

    @RequiredReadAction
    public static PsiElement findDocComment(PsiElement element, PsiElement context) {
        PsiElement docComment = null;
        boolean skippedExprStatementOnce = false;

        if (element instanceof JSAttributeListOwner jsClass && context == null) {
            JSAttributeList attributeList = jsClass.getAttributeList();
            PsiElement anchor = null;

            if (attributeList != null) {
                for (ASTNode currentNode = attributeList.getNode().getLastChildNode(); currentNode != null;
                     currentNode = currentNode.getTreePrev()) {
                    IElementType nodeType = currentNode.getElementType();

                    if (!JSTokenTypes.MODIFIERS.contains(nodeType)
                        && nodeType != JSTokenTypes.WHITE_SPACE
                        && nodeType != JSElementTypes.REFERENCE_EXPRESSION // namespace
                    ) {
                        ASTNode nextNode = currentNode.getTreeNext();
                        if (nextNode != null) {
                            anchor = nextNode.getPsi();
                        }
                        break;
                    }
                }
            }

            if (anchor != null) {
                element = anchor;
            }
        }

        if (!element.isValid() || element.getContainingFile() == null) {
            return docComment;
        }

        boolean shouldSkipPrevExprStatement = false;
        String propName = getPropertyNameFromExprStatement(element);
        if (propName != null) {
            shouldSkipPrevExprStatement = true;
        }

        for (PsiElement prev = element.getPrevSibling(); prev != null; prev = prev.getPrevSibling()) {
            if (prev instanceof PsiWhiteSpace) {
                continue;
            }

            if (prev instanceof PsiComment) {
                docComment = prev;
                if (((PsiComment)prev).getTokenType() == JSTokenTypes.DOC_COMMENT) {
                    break;
                }
            }
            else if (shouldSkipPrevExprStatement &&
                prev instanceof JSExpressionStatement &&
                !skippedExprStatementOnce) {
                String propNameFromPrev = getPropertyNameFromExprStatement(prev);
                if (!propName.equals(propNameFromPrev)) {
                    break;
                }
                skippedExprStatementOnce = true; // presumably another accessor definition
                continue;
            }

            break;
        }

        if (docComment != null) {
            while (docComment.getPrevSibling() instanceof OuterLanguageElement) {
                PsiElement siblingSibling = docComment.getPrevSibling().getPrevSibling();

                if (siblingSibling != null && siblingSibling instanceof PsiComment) {
                    docComment = siblingSibling;
                }
                else {
                    break;
                }
            }
        }
        return docComment;
    }

    @Nullable
    @RequiredReadAction
    private static String getPropertyNameFromExprStatement(@Nonnull PsiElement element) {
        String propName = null;

        if (element instanceof JSExpressionStatement expressionStatement
            && expressionStatement.getExpression() instanceof JSAssignmentExpression assignment
            && assignment.getROperand() instanceof JSFunctionExpression functionExpr) {
            String name = functionExpr.getName();
            if (name != null && (StringUtil.startsWith(name, "get") || StringUtil.startsWith(name, "set"))) {
                propName = name.substring(3);
            }
        }
        return propName;
    }

    private static String findTypeFromParameter(JSVariable parameter, PsiElement docComment) {
        if (docComment != null) {
            String[] detectedType = new String[1];

            processDocumentationTextFromComment(
                docComment.getNode(),
                new JSDocumentationProcessor() {
                    String name = parameter.getName();
                    boolean isparameter = parameter instanceof JSParameter;

                    @Override
                    public boolean needsPlainCommentData() {
                        return false;
                    }

                    @Override
                    public boolean onCommentLine(@Nonnull String line) {
                        return true;
                    }

                    @Override
                    public boolean onPatternMatch(
                        @Nonnull MetaDocType type,
                        @Nullable String matchName,
                        @Nullable String matchValue,
                        @Nullable String remainingLineContent,
                        @Nonnull String line,
                        String patternMatched
                    ) {
                        if (isparameter && type == MetaDocType.PARAMETER && matchName != null && matchName.equals(name)) {
                            detectedType[0] = matchValue;
                            return false;
                        }
                        else if (type == MetaDocType.TYPE) {
                            detectedType[0] = matchName;
                            return false;
                        }
                        return true;
                    }
                }
            );

            return detectedType[0];
        }
        return null;
    }

    public static final TokenSet PRIMITIVE_TYPE_FILTER = TokenSet.create(
        JSTokenTypes.INT_KEYWORD,
        JSTokenTypes.UINT_KEYWORD,
        JSTokenTypes.VOID_KEYWORD,
        JSTokenTypes.ANY_IDENTIFIER
    );
    public static final TokenSet TYPE_FILTER =
        TokenSet.orSet(TokenSet.create(JSElementTypes.REFERENCE_EXPRESSION), PRIMITIVE_TYPE_FILTER);

    @RequiredReadAction
    private static String findDocForAnchor(final PsiElement _anchor, final JSDocumentationProcessor.MetaDocType... expectedTypes) {
        PsiElement anchor = _anchor;
        if (_anchor instanceof JSExpression) {
            anchor = PsiTreeUtil.getParentOfType(_anchor, JSStatement.class, JSProperty.class);
        }

        if (anchor != null) {
            PsiElement docComment = findDocComment(anchor);

            if (docComment != null) {
                final SimpleReference<String> detectedType = new SimpleReference<>();

                processDocumentationTextFromComment(
                    docComment.getNode(),
                    new JSDocumentationProcessor() {
                        @Override
                        public boolean needsPlainCommentData() {
                            return false;
                        }

                        @Override
                        public boolean onCommentLine(@Nonnull String line) {
                            return true;
                        }

                        @Override
                        @RequiredReadAction
                        public boolean onPatternMatch(
                            @Nonnull MetaDocType type,
                            @Nullable String matchName,
                            @Nullable String matchValue,
                            @Nullable String remainingLineContent,
                            @Nonnull String line,
                            String patternMatched
                        ) {
                            for (MetaDocType expectedType : expectedTypes) {
                                if (type == expectedType) {
                                    if (type == MetaDocType.TYPE && _anchor instanceof JSFunction function) {
                                        JSParameterList jsParameterList = function.getParameterList();
                                        if (jsParameterList != null && jsParameterList.getParameters().length > 0) {
                                            return true;
                                        }
                                        if (_anchor.getParent() instanceof JSProperty) {
                                            return true;
                                        }
                                    }
                                    else if (type == MetaDocType.RETURN) {
                                        //if (!isOurDocComment()) return true;
                                    }

                                    detectedType.set(matchName);
                                    return false;
                                }
                            }
                            return true;
                        }
                    }
                );

                return detectedType.get();
            }
        }
        return null;
    }

    @RequiredReadAction
    public static String findType(PsiElement def) {
        return findDocForAnchor(def, JSDocumentationProcessor.MetaDocType.TYPE);
    }

    @RequiredReadAction
    public static boolean isDeprecated(PsiElement element) {
        if (element instanceof JSClass jsClass) {
            return jsClass.isDeprecated();
        }
        else if (element instanceof JSFunction function) {
            return function.isDeprecated();
        }
        else if (element instanceof JSVariable variable) {
            return variable.isDeprecated();
        }
        else if (element instanceof JSNamespaceDeclaration namespaceDeclaration) {
            return namespaceDeclaration.isDeprecated();
        }

        return calculateDeprecated(element);
    }

    @RequiredReadAction
    public static boolean calculateDeprecated(PsiElement element) {
        if (element instanceof JSExpression) {
            element = PsiTreeUtil.getParentOfType(element, JSStatement.class, JSProperty.class);
        }

        PsiElement docComment = element != null ? findDocComment(element) : null;
        if (docComment != null) {
            final boolean[] deprecatedStatus = new boolean[1];

            processDocumentationTextFromComment(
                docComment.getNode(),
                new JSDocumentationProcessor() {
                    @Override
                    public boolean needsPlainCommentData() {
                        return false;
                    }

                    @Override
                    public boolean onCommentLine(@Nonnull String line) {
                        return true;
                    }

                    @Override
                    public boolean onPatternMatch(
                        @Nonnull MetaDocType type,
                        @Nullable String matchName,
                        @Nullable String matchValue,
                        @Nullable String remainingLineContent,
                        @Nonnull String line,
                        String patternMatched
                    ) {
                        if (type == MetaDocType.DEPRECATED) {
                            deprecatedStatus[0] = true;
                            return false;
                        }
                        return true;
                    }
                }
            );

            return deprecatedStatus[0];
        }
        return false;
    }

    public static void appendHyperLinkToElement(
        @Nullable PsiElement element,
        String elementName,
        StringBuilder buffer,
        String presentableName,
        @Nullable String presentableFileName
    ) {
        PsiFile containingFile = element != null ? element.getContainingFile() : null;
        String fileName = containingFile == null
            ? null
            : !JSResolveUtil.isPredefinedFile(containingFile)
            ? containingFile.getVirtualFile().getPresentableUrl()
            : containingFile.getViewProvider().getVirtualFile().getName();

        DocumentationManagerUtil.createHyperlink(
            buffer,
            (fileName != null ? fileName + ":" : "") + elementName + (element != null ? ":" + element
                .getTextOffset() : ""),
            presentableName + (presentableFileName != null ? " in " + presentableFileName : ""),
            true
        );
    }

    @RequiredReadAction
    private static String evaluateTypeFromParameter(JSParameter parameter) {
        String s = evaluateTypeFromVariable(parameter);

        if (s == null) {
            s = findTypeFromParameter(parameter, findFunctionComment(parameter));
        }

        return s;
    }

    @RequiredReadAction
    private static PsiElement findFunctionComment(JSParameter parameter) {
        PsiElement anchor = PsiTreeUtil.getParentOfType(parameter, JSFunction.class);

        if (anchor instanceof JSFunctionExpression) {
            anchor = PsiTreeUtil.getParentOfType(anchor, JSStatement.class, JSProperty.class);
        }

        if (anchor != null) {
            return findDocComment(anchor);
        }
        return null;
    }

    @RequiredReadAction
    private static String evaluateTypeFromFunction(JSFunction function) {
        ASTNode lastCommentInFunctionBody = findTrailingCommentInFunctionBody(function);

        String typeString = null;
        if (lastCommentInFunctionBody != null) {
            typeString = unwrapCommentDelimiters(lastCommentInFunctionBody.getText()).trim();
        }

        if (typeString == null) {
            typeString = findDocForAnchor(function, JSDocumentationProcessor.MetaDocType.RETURN, JSDocumentationProcessor.MetaDocType.TYPE);
        }
        return typeString;
    }

    @RequiredReadAction
    private static String evaluateTypeFromVariable(JSVariable variable) {
        PsiElement prevSibling = variable.getFirstChild();
        if (prevSibling != null && prevSibling.getNode().getElementType() == JSTokenTypes.IDENTIFIER) {
            prevSibling = variable.getPrevSibling();
        }

        if (prevSibling instanceof PsiWhiteSpace) {
            prevSibling = prevSibling.getPrevSibling();
        }

        if (prevSibling instanceof PsiComment && prevSibling.getNode().getElementType() != JSTokenTypes.END_OF_LINE_COMMENT) {
            String parameterCommentText = prevSibling.getText();
            parameterCommentText = unwrapCommentDelimiters(parameterCommentText).trim();

            if (parameterCommentText.length() > 0
                && (Character.isUpperCase(parameterCommentText.charAt(0)) || parameterCommentText.indexOf(' ') == -1)) {
                return parameterCommentText;
            }
        }

        if (prevSibling != null && prevSibling.getNode() != null && prevSibling.getNode().getElementType() == JSTokenTypes.VAR_KEYWORD) {
            prevSibling = variable.getParent().getPrevSibling();

            if (prevSibling instanceof PsiWhiteSpace) {
                prevSibling = prevSibling.getPrevSibling();
            }

            if (prevSibling instanceof PsiComment) {
                return findTypeFromParameter(variable, prevSibling);
            }

        }
        return null;
    }

    @RequiredReadAction
    public static String findTypeFromComments(JSNamedElement element) {
        if (element instanceof JSParameter parameter) {
            return evaluateTypeFromParameter(parameter);
        }
        else if (element instanceof JSVariable variable) {
            return evaluateTypeFromVariable(variable);
        }
        else if (element instanceof JSFunction function) {
            return evaluateTypeFromFunction(function);
        }
        return null;
    }

    public static boolean isSymbolReference(String content) {
        int i = content.indexOf(' ');
        if (i == -1) {
            return false;
        }
        String text = content.substring(0, i);
        return text.indexOf('.') != -1 || text.indexOf('#') != -1;
    }

    @RequiredReadAction
    public static boolean findOptionalStatusFromComments(final JSParameter parameter) {
        PsiElement docComment = findFunctionComment(parameter);
        if (docComment == null) {
            return false;
        }

        final boolean[] detectedType = new boolean[1];

        processDocumentationTextFromComment(
            docComment.getNode(),
            new JSDocumentationProcessor() {
                final String name = parameter.getName();

                @Override
                public boolean needsPlainCommentData() {
                    return false;
                }

                @Override
                public boolean onCommentLine(@Nonnull String line) {
                    return true;
                }

                @Override
                public boolean onPatternMatch(
                    @Nonnull MetaDocType type,
                    @Nullable String matchName,
                    @Nullable String matchValue,
                    @Nullable String remainingLineContent,
                    @Nonnull String line,
                    String patternMatched
                ) {
                    if (type == MetaDocType.OPTIONAL_PARAMETERS && matchName != null && matchName.equals(name) && matchValue == null) {
                        detectedType[0] = true;
                        return false;
                    }

                    return true;
                }
            }
        );

        return detectedType[0];
    }
}
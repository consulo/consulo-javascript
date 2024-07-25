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
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.ast.TokenSet;
import consulo.language.ast.TokenType;
import consulo.language.editor.documentation.DocumentationManagerUtil;
import consulo.language.psi.*;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.util.lang.StringUtil;
import consulo.util.lang.ref.Ref;
import org.jetbrains.annotations.NonNls;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class JSDocumentationUtils {
    @NonNls
    private static final Pattern ourDojoParametersPattern = Pattern.compile("^\\s*(\\w+):(.*)$");
    @NonNls
    private static final Pattern ourJSDocParametersPattern =
        Pattern.compile("^\\s*@param\\s*(?:\\{([^\\}]+)\\}\\s*)?(\\w+)?(?:\\s:\\s(\\S+))?(?:\\s*\\[(\\w+)(?:\\.(\\w+))?" +
            "(?:=([^\\]]*))?\\])?(.*)$");
    @NonNls
    private static final Pattern ourJSDocEventPattern = Pattern.compile("^\\s*@event\\s*(\\w+)(?:\\s:\\s(\\S+))?(.*)$");
    @NonNls
    private static final Pattern ourJSDocRemarkPattern = Pattern.compile("^\\s*@remarks (.*)$");
    @NonNls
    private static final Pattern ourJSDocMethodPattern = Pattern.compile("^\\s*@method\\s*(\\w+)(.*)$");
    @NonNls
    private static final Pattern ourJSDocClassPattern = Pattern.compile("^\\s*@class\\s*(\\w+(?:\\.\\w+)*)(.*)$");
    @NonNls
    private static final Pattern ourJSDocDeprecatedPattern = Pattern.compile("^\\s*@deprecated\\s*(.*)$");
    @NonNls
    private static final Pattern ourJSDocConstructorPattern = Pattern.compile("^\\s*@constructor$");
    @NonNls
    private static final Pattern ourJSDocFinalPattern = Pattern.compile("^\\s*@final$");
    @NonNls
    private static final Pattern ourJSDocPrivatePattern = Pattern.compile("^\\s*@private$");
    @NonNls
    private static final Pattern ourJSDocPublicPattern = Pattern.compile("^\\s*@public$");
    @NonNls
    private static final Pattern ourJSDocProtectedPattern = Pattern.compile("^\\s*@protected$");
    @NonNls
    private static final Pattern ourJSDocOptionalPattern = Pattern.compile("^\\s*@optional(.*)$");
    @NonNls
    private static final Pattern ourJSDocStaticPattern = Pattern.compile("^\\s*@static$");
    @NonNls
    private static final Pattern ourJSDocSeePattern = Pattern.compile("^\\s*@see (.*)$");
    @NonNls
    private static final Pattern ourJSDocDescriptionPattern = Pattern.compile("^\\s*@description\\s*(.+)$");
    @NonNls
    private static final Pattern ourJSDocReturnPattern =
        Pattern.compile("^\\s*@return(?:s)?\\s*(?:(?:\\{|:)?\\s*([^\\s\\}]+)\\s*\\}?\\s*)?(.*)$");
    @NonNls
    private static final Pattern ourJSDocNamespacePattern = Pattern.compile("^\\s*@namespace\\s*([\\w\\.]+)(.*)$");
    @NonNls
    private static final Pattern ourJSDocPropertyPattern = Pattern.compile("^\\s*@property\\s*(\\w+)(.*)$");
    @NonNls
    private static final Pattern ourJSDocTypePattern = Pattern.compile("^\\s*@type\\s*\\{?([^\\s\\}]+)\\}?(.*)$");
    @NonNls
    private static final Pattern ourJSDocRequiresPattern = Pattern.compile("^\\s*@requires\\s*(\\S+)(.*)$");
    @NonNls
    private static final Pattern ourJSDocDefaultPattern = Pattern.compile("^\\s*@default\\s*(.*)$");
    @NonNls
    private static final Pattern ourJSDocExtendsPattern = Pattern.compile("^\\s*@extends\\s*(.*)$");
    @NonNls
    private static final Map<Pattern, String> patternToHintMap = new HashMap<>();
    @NonNls
    private static final Map<Pattern, JSDocumentationProcessor.MetaDocType> patternToMetaDocTypeMap = new HashMap<>();

    static {
        patternToHintMap.put(ourDojoParametersPattern, ":");
        patternToMetaDocTypeMap.put(ourDojoParametersPattern, JSDocumentationProcessor.MetaDocType.PARAMETER);
        patternToHintMap.put(ourJSDocParametersPattern, "@pa");
        patternToMetaDocTypeMap.put(ourJSDocParametersPattern, JSDocumentationProcessor.MetaDocType.PARAMETER);

        patternToHintMap.put(ourJSDocMethodPattern, "@m");
        patternToMetaDocTypeMap.put(ourJSDocMethodPattern, JSDocumentationProcessor.MetaDocType.METHOD);

        patternToHintMap.put(ourJSDocOptionalPattern, "@o");
        patternToMetaDocTypeMap.put(ourJSDocOptionalPattern, JSDocumentationProcessor.MetaDocType.OPTIONAL_PARAMETERS);

        patternToHintMap.put(ourJSDocEventPattern, "@ev");
        patternToMetaDocTypeMap.put(ourJSDocEventPattern, JSDocumentationProcessor.MetaDocType.EVENT);

        patternToHintMap.put(ourJSDocExtendsPattern, "@ex");
        patternToMetaDocTypeMap.put(ourJSDocExtendsPattern, JSDocumentationProcessor.MetaDocType.EXTENDS);

        patternToHintMap.put(ourJSDocRemarkPattern, "@rem");
        patternToMetaDocTypeMap.put(ourJSDocRemarkPattern, JSDocumentationProcessor.MetaDocType.NOTE);

        patternToHintMap.put(ourJSDocReturnPattern, "@ret");
        patternToMetaDocTypeMap.put(ourJSDocReturnPattern, JSDocumentationProcessor.MetaDocType.RETURN);

        patternToHintMap.put(ourJSDocPublicPattern, "@pu");
        patternToMetaDocTypeMap.put(ourJSDocPublicPattern, JSDocumentationProcessor.MetaDocType.PUBLIC);

        patternToHintMap.put(ourJSDocProtectedPattern, "@prot");
        patternToMetaDocTypeMap.put(ourJSDocProtectedPattern, JSDocumentationProcessor.MetaDocType.PROTECTED);

        patternToHintMap.put(ourJSDocStaticPattern, "@st");
        patternToMetaDocTypeMap.put(ourJSDocStaticPattern, JSDocumentationProcessor.MetaDocType.STATIC);

        patternToHintMap.put(ourJSDocSeePattern, "@se");
        patternToMetaDocTypeMap.put(ourJSDocSeePattern, JSDocumentationProcessor.MetaDocType.SEE);

        patternToHintMap.put(ourJSDocDescriptionPattern, "@des");
        patternToMetaDocTypeMap.put(ourJSDocDescriptionPattern, JSDocumentationProcessor.MetaDocType.DESCRIPTION);

        patternToHintMap.put(ourJSDocDeprecatedPattern, "@dep");
        patternToMetaDocTypeMap.put(ourJSDocDeprecatedPattern, JSDocumentationProcessor.MetaDocType.DEPRECATED);

        patternToHintMap.put(ourJSDocConstructorPattern, "@co");

        patternToMetaDocTypeMap.put(ourJSDocConstructorPattern, JSDocumentationProcessor.MetaDocType.CONSTRUCTOR);

        patternToHintMap.put(ourJSDocClassPattern, "@cl");
        patternToMetaDocTypeMap.put(ourJSDocClassPattern, JSDocumentationProcessor.MetaDocType.CLASS);
        patternToHintMap.put(ourJSDocPrivatePattern, "@pri");
        patternToMetaDocTypeMap.put(ourJSDocPrivatePattern, JSDocumentationProcessor.MetaDocType.PRIVATE);

        patternToHintMap.put(ourJSDocNamespacePattern, "@n");
        patternToMetaDocTypeMap.put(ourJSDocNamespacePattern, JSDocumentationProcessor.MetaDocType.NAMESPACE);

        patternToHintMap.put(ourJSDocPropertyPattern, "@prop");
        patternToHintMap.put(ourJSDocTypePattern, "@t");
        patternToMetaDocTypeMap.put(ourJSDocTypePattern, JSDocumentationProcessor.MetaDocType.TYPE);

        patternToHintMap.put(ourJSDocFinalPattern, "@f");
        patternToMetaDocTypeMap.put(ourJSDocFinalPattern, JSDocumentationProcessor.MetaDocType.FINAL);
        patternToHintMap.put(ourJSDocRequiresPattern, "@req");
        patternToMetaDocTypeMap.put(ourJSDocRequiresPattern, JSDocumentationProcessor.MetaDocType.REQUIRES);

        patternToHintMap.put(ourJSDocDefaultPattern, "@def");
        patternToMetaDocTypeMap.put(ourJSDocDefaultPattern, JSDocumentationProcessor.MetaDocType.DEFAULT);
    }

    private static final
    @NonNls
    Map<Pattern, String> prefixToPatternToHintMap = new HashMap<Pattern, String>();

    static {
        prefixToPatternToHintMap.put(Pattern.compile("^\\s*description:(.*)$"), "descr");

        prefixToPatternToHintMap.put(Pattern.compile("^ summary(?:\\:)?(.*)$"), "summ");

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

                    final String text = resultCommentNode.getText();
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

        final boolean needPlainCharData = processor.needsPlainCommentData();

        while (commentLineIterator.hasMoreElements()) {
            final String s = (String)commentLineIterator.nextElement();
            if (s.indexOf('@') == -1 && s.indexOf(':') == -1 && !needPlainCharData) {
                continue;
            }

            String commentText = s.replace('\t', ' ');

            for (Map.Entry<Pattern, String> entry : prefixToPatternToHintMap.entrySet()) {
                final Matcher matcher = commentText.indexOf(entry.getValue()) != -1 ? entry.getKey().matcher(commentText) : null;
                if (matcher == null) {
                    continue;
                }

                if (matcher.matches()) {
                    commentText = matcher.group(1);
                    break;
                }
            }

            boolean matchedSomething = false;

            for (Map.Entry<Pattern, String> entry : patternToHintMap.entrySet()) {
                final Matcher matcher = commentText.indexOf(entry.getValue()) != -1 ? entry.getKey().matcher(commentText) : null;
                if (matcher == null) {
                    continue;
                }

                if (matcher.matches()) {
                    final JSDocumentationProcessor.MetaDocType docType = patternToMetaDocTypeMap.get(entry.getKey());
                    if (docType != null) {
                        final int groupCount = matcher.groupCount();
                        String remainingLineContent = groupCount > 0 ? matcher.group(groupCount) : null;
                        String matchName = groupCount > 1 ? matcher.group(1) : null;
                        String matchValue = groupCount > 2 ? matcher.group(2) : null;

                        boolean reportAboutOptionalParameter = false;
                        boolean reportAboutFieldInParameter = false;
                        boolean reportAboutDefaultValue = false;

                        final int groupForInitialValue = 6;
                        final int groupForFieldName = 5;
                        String fieldName = null;

                        if (groupCount == 7 && entry.getKey() == ourJSDocParametersPattern) {
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
                        else {
                            if (!processor.onPatternMatch(docType, matchName, matchValue, remainingLineContent, commentText, matched)) {
                                break;
                            }
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

    static
    @Nullable
    ASTNode findTrailingCommentInFunctionBody(final @Nonnull JSFunction function) {
        final ASTNode block = function.getNode().findChildByType(JSElementTypes.BLOCK_STATEMENT);
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

    static
    @Nullable
    ASTNode findLeadingCommentInFunctionBody(final @Nonnull PsiElement element) {
        final ASTNode functionNode = element.getNode();
        final ASTNode block = functionNode.findChildByType(JSElementTypes.BLOCK_STATEMENT);
        if (block == null) {
            return null;
        }

        for (ASTNode node = block.getFirstChildNode().getTreeNext(); node != null; node = node.getTreeNext()) {
            final IElementType nodeType = node.getElementType();

            if (nodeType != TokenType.WHITE_SPACE) {
                if (JSTokenTypes.COMMENTS.contains(nodeType)) {
                    return node;
                }
                return null;
            }
        }

        return null;
    }

    public static PsiElement findDocComment(PsiElement element) {
        return findDocComment(element, null);
    }

    public static PsiElement findDocComment(PsiElement element, PsiElement context) {
        PsiElement docComment = null;
        boolean skippedExprStatementOnce = false;

        if (element instanceof JSAttributeListOwner jsClass && context == null) {
            final JSAttributeList attributeList = jsClass.getAttributeList();
            PsiElement anchor = null;

            if (attributeList != null) {
                for (ASTNode currentNode = attributeList.getNode().getLastChildNode(); currentNode != null;
                     currentNode = currentNode.getTreePrev()) {
                    final IElementType nodeType = currentNode.getElementType();

                    if (!JSTokenTypes.MODIFIERS.contains(nodeType)
                        && nodeType != JSTokenTypes.WHITE_SPACE
                        && nodeType != JSElementTypes.REFERENCE_EXPRESSION // namespace
                    ) {
                        final ASTNode nextNode = currentNode.getTreeNext();
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

    private static
    @Nullable
    String getPropertyNameFromExprStatement(@Nonnull PsiElement element) {
        String propName = null;

        if (element instanceof JSExpressionStatement expressionStatement
            && expressionStatement.getExpression() instanceof JSAssignmentExpression assignmentExpression
            && assignmentExpression.getROperand() instanceof JSFunctionExpression functionExpression) {
            String name = functionExpression.getName();
            if (name != null && (StringUtil.startsWith(name, "get") || (StringUtil.startsWith(name, "set")))) {
                propName = name.substring(3);
            }
        }
        return propName;
    }

    private static String findTypeFromParameter(final JSVariable parameter, final PsiElement docComment) {
        if (docComment != null) {
            final String[] detectedType = new String[1];

            processDocumentationTextFromComment(docComment.getNode(), new JSDocumentationProcessor() {
                final String name = parameter.getName();
                final boolean isparameter = parameter instanceof JSParameter;

                @Override
                public boolean needsPlainCommentData() {
                    return false;
                }

                @Override
                public boolean onCommentLine(@Nonnull final String line) {
                    return true;
                }

                @Override
                public boolean onPatternMatch(
                    @Nonnull final MetaDocType type, @Nullable final String matchName, @Nullable final String matchValue,
                    @Nullable final String remainingLineContent, @Nonnull final String line, final String patternMatched
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
            });

            return detectedType[0];
        }
        return null;
    }

    public static final TokenSet ourPrimitiveTypeFilter = TokenSet.create(JSTokenTypes.INT_KEYWORD, JSTokenTypes.UINT_KEYWORD,
        JSTokenTypes.VOID_KEYWORD, JSTokenTypes.ANY_IDENTIFIER
    );
    public static final TokenSet ourTypeFilter =
        TokenSet.orSet(TokenSet.create(JSElementTypes.REFERENCE_EXPRESSION), ourPrimitiveTypeFilter);

    private static String findDocForAnchor(final PsiElement _anchor, final JSDocumentationProcessor.MetaDocType... expectedTypes) {
        PsiElement anchor = _anchor;
        if (_anchor instanceof JSExpression) {
            anchor = PsiTreeUtil.getParentOfType(_anchor, JSStatement.class, JSProperty.class);
        }

        if (anchor != null) {
            final PsiElement docComment = findDocComment(anchor);

            if (docComment != null) {
                final Ref<String> detectedType = new Ref<>();

                final PsiElement anchor1 = anchor;
                processDocumentationTextFromComment(docComment.getNode(), new JSDocumentationProcessor() {
                    @Override
                    public boolean needsPlainCommentData() {
                        return false;
                    }

                    @Override
                    public boolean onCommentLine(@Nonnull final String line) {
                        return true;
                    }

                    @Override
                    public boolean onPatternMatch(
                        @Nonnull final MetaDocType type, @Nullable final String matchName, @Nullable final String matchValue,
                        @Nullable final String remainingLineContent, @Nonnull final String line, final String patternMatched
                    ) {
                        for (MetaDocType expectedType : expectedTypes) {
                            if (type == expectedType) {
                                if (type == MetaDocType.TYPE && _anchor instanceof JSFunction function) {
                                    final JSParameterList jsParameterList = function.getParameterList();
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
                });

                return detectedType.get();
            }
        }
        return null;
    }

    public static String findType(final PsiElement def) {
        return findDocForAnchor(def, JSDocumentationProcessor.MetaDocType.TYPE);
    }

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

    public static boolean calculateDeprecated(PsiElement element) {
        if (element instanceof JSExpression) {
            element = PsiTreeUtil.getParentOfType(element, JSStatement.class, JSProperty.class);
        }

        final PsiElement docComment = element != null ? findDocComment(element) : null;
        if (docComment != null) {
            final boolean[] deprecatedStatus = new boolean[1];

            processDocumentationTextFromComment(docComment.getNode(), new JSDocumentationProcessor() {
                @Override
                public boolean needsPlainCommentData() {
                    return false;
                }

                @Override
                public boolean onCommentLine(@Nonnull final String line) {
                    return true;
                }

                @Override
                public boolean onPatternMatch(
                    @Nonnull final MetaDocType type,
                    @Nullable final String matchName,
                    @Nullable final String matchValue,
                    @Nullable final String remainingLineContent,
                    @Nonnull final String line,
                    final String patternMatched
                ) {
                    if (type == MetaDocType.DEPRECATED) {
                        deprecatedStatus[0] = true;
                        return false;
                    }
                    return true;
                }
            });

            return deprecatedStatus[0];
        }
        return false;
    }

    public static void appendHyperLinkToElement(
        @Nullable PsiElement element,
        String elementName,
        final StringBuilder buffer,
        final String presentableName,
        final @Nullable String presentableFileName
    ) {
        final PsiFile containingFile = element != null ? element.getContainingFile() : null;
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

    private static String evaluateTypeFromParameter(final JSParameter parameter) {
        String s = evaluateTypeFromVariable(parameter);

        if (s == null) {
            s = findTypeFromParameter(parameter, findFunctionComment(parameter));
        }

        return s;
    }

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

    private static String evaluateTypeFromFunction(final JSFunction function) {
        final ASTNode lastCommentInFunctionBody = findTrailingCommentInFunctionBody(function);

        String typeString = null;
        if (lastCommentInFunctionBody != null) {
            typeString = unwrapCommentDelimiters(lastCommentInFunctionBody.getText()).trim();
        }

        if (typeString == null) {
            typeString = findDocForAnchor(function, JSDocumentationProcessor.MetaDocType.RETURN, JSDocumentationProcessor.MetaDocType.TYPE);
        }
        return typeString;
    }

    private static String evaluateTypeFromVariable(final JSVariable variable) {
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

    public static String findTypeFromComments(final JSNamedElement element) {
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

    public static boolean findOptionalStatusFromComments(final JSParameter parameter) {
        PsiElement docComment = findFunctionComment(parameter);
        if (docComment == null) {
            return false;
        }

        final boolean[] detectedType = new boolean[1];

        processDocumentationTextFromComment(docComment.getNode(), new JSDocumentationProcessor() {
            final String name = parameter.getName();

            @Override
            public boolean needsPlainCommentData() {
                return false;
            }

            @Override
            public boolean onCommentLine(@Nonnull final String line) {
                return true;
            }

            @Override
            public boolean onPatternMatch(
                @Nonnull final MetaDocType type,
                @Nullable final String matchName,
                @Nullable final String matchValue,
                @Nullable final String remainingLineContent,
                @Nonnull final String line,
                final String patternMatched
            ) {
                if (type == MetaDocType.OPTIONAL_PARAMETERS && matchName != null && matchName.equals(name) && matchValue == null) {
                    detectedType[0] = true;
                    return false;
                }

                return true;
            }
        });

        return detectedType[0];
    }
}
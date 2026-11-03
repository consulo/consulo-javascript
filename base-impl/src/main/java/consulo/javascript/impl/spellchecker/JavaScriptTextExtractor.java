/*
 * Copyright 2013-2026 consulo.io
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
package consulo.javascript.impl.spellchecker;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSDocComment;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.document.util.TextRange;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.ast.IElementType;
import consulo.language.ast.TokenSet;
import consulo.language.psi.PsiComment;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiUtilCore;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.spellcheker.text.TextContent;
import consulo.language.spellcheker.text.TextContentBuilder;
import consulo.language.spellcheker.text.TextExtractor;
import consulo.language.spellcheker.text.util.EscapeUtils;
import consulo.language.spellcheker.text.util.HtmlUtils;
import consulo.language.spellcheker.text.util.PsiUtils;
import consulo.language.spellcheker.text.util.Text;
import consulo.util.collection.ContainerUtil;
import org.jspecify.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static consulo.language.spellcheker.text.TextContent.TextDomain.COMMENTS;
import static consulo.language.spellcheker.text.TextContent.TextDomain.DOCUMENTATION;
import static consulo.language.spellcheker.text.TextContent.TextDomain.LITERALS;

/**
 * Natural language text in JavaScript (and dialects based on it):
 * <ul>
 *   <li>JSDoc comments: the description and the descriptions of block tags; tag names, type expressions,
 *   parameter names and non-prose tags are excluded, inline tags and code spans are unknown fragments,</li>
 *   <li>{@code //} (adjacent ones joined) and {@code /* *}{@code /} comments, except tool directives,</li>
 *   <li>string and template literals without quotes; template substitutions are unknown fragments;
 *   module specifiers, object keys, compared values and tagged templates are skipped.</li>
 * </ul>
 */
@ExtensionImpl
public class JavaScriptTextExtractor extends TextExtractor {
    private static final TokenSet STRING_LITERALS = TokenSet.create(JSTokenTypes.STRING_LITERAL, JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL);

    private static final TextContentBuilder COMMENT_BUILDER =
        TextContentBuilder.FromPsi.removingIndents(" \t*").removingLineSuffixes(" \t");

    private static final Pattern BLOCK_TAG = Pattern.compile("(?m)^[ \t]*@([A-Za-z][\\w-]*)");
    private static final Pattern INLINE_UNKNOWN = Pattern.compile("\\{@[^}]*}|`[^`\n]*`");
    private static final Pattern LINE_CONTINUATION = Pattern.compile("\\\\\n");
    private static final Pattern DIRECTIVE = Pattern.compile(
        "^\\s*(?:eslint|jshint|jslint|jscs|tslint|globals?\\b|exported\\b|istanbul\\b|c8\\b|prettier-ignore|@ts-|@flow|@jsx|"
            + "@vite|webpackChunkName|webpackIgnore|#\\s*source(?:Mapping)?URL|@source(?:Mapping)?URL|<reference|noinspection)"
    );

    private static final Set<String> PROSE_TAGS = Set.of(
        "param", "arg", "argument", "property", "prop", "template",
        "return", "returns", "throws", "exception", "yield", "yields",
        "deprecated", "description", "desc", "summary", "classdesc", "todo"
    );
    private static final Set<String> NAMED_TAGS = Set.of("param", "arg", "argument", "property", "prop", "template");

    private static final Set<String> SKIPPED_BEFORE_LITERAL = Set.of(
        "from", "import", "case", "==", "===", "!=", "!=="
    );

    @Override
    @RequiredReadAction
    protected List<TextContent> buildTextContents(PsiElement element, Set<TextContent.TextDomain> allowedDomains) {
        if (element instanceof JSDocComment) {
            if (!allowedDomains.contains(DOCUMENTATION)) {
                return List.of();
            }
            return buildDocumentation(element);
        }

        IElementType type = PsiUtilCore.getElementType(element);
        if (type == null) {
            return List.of();
        }

        if (element instanceof PsiComment) {
            if (!allowedDomains.contains(COMMENTS) || (type != JSTokenTypes.END_OF_LINE_COMMENT && type != JSTokenTypes.C_STYLE_COMMENT)) {
                return List.of();
            }
            return buildComments(element, type);
        }

        if (STRING_LITERALS.contains(type)) {
            if (!allowedDomains.contains(LITERALS) || isNotProseLiteral(element)) {
                return List.of();
            }
            return buildStringLiteral(element);
        }

        if (isStringTemplate(element)) {
            if (!allowedDomains.contains(LITERALS) || isTaggedTemplate(element)) {
                return List.of();
            }
            return buildStringTemplate(element);
        }
        return List.of();
    }

    @RequiredReadAction
    private static List<TextContent> buildComments(PsiElement element, IElementType type) {
        List<PsiElement> roots = PsiUtils.getNotSoDistantSimilarSiblings(
            element,
            it -> it instanceof PsiComment && !(it instanceof JSDocComment) && PsiUtilCore.getElementType(it) == type
        );
        List<TextContent> fragments = new ArrayList<>();
        for (PsiElement root : roots) {
            TextContent fragment = buildCommentBody(root, COMMENTS);
            if (fragment != null && !DIRECTIVE.matcher(fragment).find()) {
                fragments.add(fragment);
            }
        }
        return ContainerUtil.createMaybeSingletonList(TextContent.joinWithWhitespace('\n', fragments));
    }

    /**
     * The comment text without its {@code //}, {@code /*}, {@code /**} and {@code *}{@code /} delimiters and leading asterisks.
     */
    @RequiredReadAction
    private static @Nullable TextContent buildCommentBody(PsiElement comment, TextContent.TextDomain domain) {
        String text = comment.getText();
        int start = 0;
        int end = text.length();
        if (text.startsWith("//")) {
            start = 2;
        }
        else if (text.startsWith("/*")) {
            start = text.startsWith("/**") && end >= 5 ? 3 : 2;
            if (end >= 4 && text.endsWith("*/")) {
                end -= 2;
            }
        }
        if (start >= end) {
            return null;
        }
        return COMMENT_BUILDER.build(comment, domain, new TextRange(start, end));
    }

    @RequiredReadAction
    private static List<TextContent> buildDocumentation(PsiElement comment) {
        TextContent content = buildCommentBody(comment, DOCUMENTATION);
        if (content == null) {
            return List.of();
        }

        String text = content.toString();
        List<TextRange> proseRanges = new ArrayList<>();
        Matcher matcher = BLOCK_TAG.matcher(text);
        int descriptionEnd = text.length();
        List<int[]> tags = new ArrayList<>();
        while (matcher.find()) {
            if (tags.isEmpty()) {
                descriptionEnd = matcher.start();
            }
            tags.add(new int[]{matcher.start(1), matcher.end(1)});
        }

        proseRanges.add(new TextRange(0, descriptionEnd));
        for (int i = 0; i < tags.size(); i++) {
            int nameStart = tags.get(i)[0];
            int nameEnd = tags.get(i)[1];
            int bodyEnd = i + 1 < tags.size() ? tags.get(i + 1)[0] - 1 : text.length();
            String name = text.substring(nameStart, nameEnd);
            if (!PROSE_TAGS.contains(name)) {
                continue;
            }
            int start = skipWhitespace(text, nameEnd, bodyEnd);
            start = skipBalanced(text, start, bodyEnd, '{', '}');
            if (NAMED_TAGS.contains(name)) {
                start = skipWhitespace(text, start, bodyEnd);
                int afterBrackets = skipBalanced(text, start, bodyEnd, '[', ']');
                start = afterBrackets != start ? afterBrackets : skipWord(text, start, bodyEnd);
            }
            start = skipWhitespace(text, start, bodyEnd);
            if (start < bodyEnd && text.charAt(start) == '-') {
                start++;
            }
            if (start < bodyEnd) {
                proseRanges.add(new TextRange(start, bodyEnd));
            }
        }

        List<TextContent> result = new ArrayList<>();
        for (TextRange range : proseRanges) {
            TextContent part = content.subText(range);
            if (part == null) {
                continue;
            }
            part = part.excludeRanges(ContainerUtil.map(Text.allOccurrences(INLINE_UNKNOWN, part), TextContent.Exclusion::markUnknown));
            TextContent trimmed = part.trimWhitespace();
            if (trimmed != null) {
                result.addAll(HtmlUtils.excludeHtml(trimmed));
            }
        }
        return result;
    }

    @RequiredReadAction
    private static List<TextContent> buildStringLiteral(PsiElement literal) {
        TextContent content = TextContentBuilder.FromPsi.build(literal, LITERALS, new TextRange(0, literal.getTextLength()));
        if (content == null) {
            return List.of();
        }
        TextContent result = content;
        if (result.length() > 1 && isQuote(result.charAt(result.length() - 1))) {
            result = result.excludeRange(new TextRange(result.length() - 1, result.length()));
        }
        if (result.length() > 0 && isQuote(result.charAt(0))) {
            result = result.excludeRange(new TextRange(0, 1));
        }
        return normalizeEscapes(result);
    }

    @RequiredReadAction
    private static List<TextContent> buildStringTemplate(PsiElement template) {
        TextContentBuilder builder = TextContentBuilder.FromPsi
            .withUnknown(e -> e.getParent() == template && PsiUtilCore.getElementType(e) == JSTokenTypes.DOLLAR)
            .excluding(e -> e.getParent() == template && PsiUtilCore.getElementType(e) != JSTokenTypes.STRING_TEMPLATE_PART);
        TextContent content = builder.build(template, LITERALS, new TextRange(0, template.getTextLength()));
        if (content == null) {
            return List.of();
        }
        return normalizeEscapes(content);
    }

    private static List<TextContent> normalizeEscapes(TextContent content) {
        if (content.length() == 0) {
            return List.of();
        }
        TextContent result = content.excludeRanges(
            ContainerUtil.map(Text.allOccurrences(LINE_CONTINUATION, content), TextContent.Exclusion::exclude)
        );
        result = EscapeUtils.replaceBackslashEscapes(result);
        return ContainerUtil.createMaybeSingletonList(result.trimWhitespace());
    }

    @RequiredReadAction
    private static boolean isStringTemplate(PsiElement element) {
        PsiElement first = element.getFirstChild();
        return first != null && first != element.getLastChild() && PsiUtilCore.getElementType(first) == JSTokenTypes.BACKQUOTE;
    }

    @RequiredReadAction
    private static boolean isTaggedTemplate(PsiElement template) {
        PsiElement prev = PsiTreeUtil.skipWhitespacesAndCommentsBackward(template);
        return prev != null && (prev.getFirstChild() != null || PsiUtilCore.getElementType(prev) == JSTokenTypes.IDENTIFIER);
    }

    /**
     * Module specifiers ({@code import x from "y"}, {@code require("y")}), object keys ({@code {"key": 1}}),
     * index keys and values compared with something are identifiers rather than prose.
     */
    @RequiredReadAction
    private static boolean isNotProseLiteral(PsiElement literal) {
        PsiElement prev = PsiTreeUtil.prevVisibleLeaf(literal);
        PsiElement next = PsiTreeUtil.nextVisibleLeaf(literal);
        String prevText = prev == null ? "" : prev.getText();
        String nextText = next == null ? "" : next.getText();
        if (SKIPPED_BEFORE_LITERAL.contains(prevText)) {
            return true;
        }
        if (nextText.equals("==") || nextText.equals("===") || nextText.equals("!=") || nextText.equals("!==") || nextText.equals("in")) {
            return true;
        }
        if (prevText.equals("[") && nextText.equals("]")) {
            return true;
        }
        if (nextText.equals(":") && (prevText.equals("{") || prevText.equals(","))) {
            return true;
        }
        if (prevText.equals("(") && prev != null) {
            PsiElement callee = PsiTreeUtil.prevVisibleLeaf(prev);
            String calleeText = callee == null ? "" : callee.getText();
            return calleeText.equals("require") || calleeText.equals("import");
        }
        return false;
    }

    private static boolean isQuote(char c) {
        return c == '"' || c == '\'';
    }

    private static int skipWhitespace(String text, int start, int end) {
        int i = start;
        while (i < end && Character.isWhitespace(text.charAt(i))) {
            i++;
        }
        return i;
    }

    private static int skipWord(String text, int start, int end) {
        int i = start;
        while (i < end && !Character.isWhitespace(text.charAt(i))) {
            i++;
        }
        return i;
    }

    private static int skipBalanced(String text, int start, int end, char open, char close) {
        if (start >= end || text.charAt(start) != open) {
            return start;
        }
        int depth = 0;
        for (int i = start; i < end; i++) {
            char c = text.charAt(i);
            if (c == open) {
                depth++;
            }
            else if (c == close) {
                depth--;
                if (depth == 0) {
                    return i + 1;
                }
            }
        }
        return end;
    }

    @Override
    public Language getLanguage() {
        return JavaScriptLanguage.INSTANCE;
    }
}

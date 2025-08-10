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

package com.intellij.lang.javascript.impl.highlighting;

import consulo.annotation.component.ExtensionImpl;
import consulo.colorScheme.TextAttributesKey;
import consulo.colorScheme.setting.AttributesDescriptor;
import consulo.javascript.ide.hightlight.JavaScriptSyntaxHighlightKeys;
import consulo.javascript.lang.JavaScript15LanguageVersion;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.editor.colorScheme.setting.ColorSettingsPage;
import consulo.language.editor.highlight.SyntaxHighlighter;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Maxim.Mossienko
 * @since 2005-11-02
 */
@ExtensionImpl
public class JavaScriptColorsAndFontsPage implements ColorSettingsPage {
    private static final AttributesDescriptor[] ATTRS;

    private static Map<String, TextAttributesKey> ADDITIONAL_HIGHLIGHT_DESCRIPTORS = new HashMap<>();

    static {
        ATTRS = new AttributesDescriptor[]{
            new AttributesDescriptor(JavaScriptLocalize.javascriptKeyword(), JavaScriptSyntaxHighlightKeys.JS_KEYWORD),
            new AttributesDescriptor(JavaScriptLocalize.javascriptString(), JavaScriptSyntaxHighlightKeys.JS_STRING),
            new AttributesDescriptor(
                JavaScriptLocalize.javascriptValidStringEscape(),
                JavaScriptSyntaxHighlightKeys.JS_VALID_STRING_ESCAPE
            ),
            new AttributesDescriptor(
                JavaScriptLocalize.javascriptInvalidStringEscape(),
                JavaScriptSyntaxHighlightKeys.JS_INVALID_STRING_ESCAPE
            ),
            new AttributesDescriptor(JavaScriptLocalize.javascriptNumber(), JavaScriptSyntaxHighlightKeys.JS_NUMBER),
            new AttributesDescriptor(JavaScriptLocalize.javascriptRegexp(), JavaScriptSyntaxHighlightKeys.JS_REGEXP),
            new AttributesDescriptor(JavaScriptLocalize.javascriptLinecomment(), JavaScriptSyntaxHighlightKeys.JS_LINE_COMMENT),
            new AttributesDescriptor(JavaScriptLocalize.javascriptBlockcomment(), JavaScriptSyntaxHighlightKeys.JS_BLOCK_COMMENT),
            new AttributesDescriptor(JavaScriptLocalize.javascriptDoccomment(), JavaScriptSyntaxHighlightKeys.JS_DOC_COMMENT),
            new AttributesDescriptor(JavaScriptLocalize.javascriptOperation(), JavaScriptSyntaxHighlightKeys.JS_OPERATION_SIGN),
            new AttributesDescriptor(JavaScriptLocalize.javascriptParens(), JavaScriptSyntaxHighlightKeys.JS_PARENTHS),
            new AttributesDescriptor(JavaScriptLocalize.javascriptBrackets(), JavaScriptSyntaxHighlightKeys.JS_BRACKETS),
            new AttributesDescriptor(JavaScriptLocalize.javascriptBraces(), JavaScriptSyntaxHighlightKeys.JS_BRACES),
            new AttributesDescriptor(JavaScriptLocalize.javascriptComma(), JavaScriptSyntaxHighlightKeys.JS_COMMA),
            new AttributesDescriptor(JavaScriptLocalize.javascriptDot(), JavaScriptSyntaxHighlightKeys.JS_DOT),
            new AttributesDescriptor(JavaScriptLocalize.javascriptSemicolon(), JavaScriptSyntaxHighlightKeys.JS_SEMICOLON),
            new AttributesDescriptor(JavaScriptLocalize.javascriptBadcharacter(), JavaScriptSyntaxHighlightKeys.JS_BAD_CHARACTER),
            new AttributesDescriptor(JavaScriptLocalize.javascriptDocmarkup(), JavaScriptSyntaxHighlightKeys.JS_DOC_MARKUP),
            new AttributesDescriptor(JavaScriptLocalize.javascriptDoctag(), JavaScriptSyntaxHighlightKeys.JS_DOC_TAG),
            new AttributesDescriptor(JavaScriptLocalize.javascriptParameter(), JavaScriptSyntaxHighlightKeys.JS_PARAMETER),
            new AttributesDescriptor(JavaScriptLocalize.javascriptLocalVariable(), JavaScriptSyntaxHighlightKeys.JS_LOCAL_VARIABLE),
            new AttributesDescriptor(JavaScriptLocalize.javascriptGlobalVariable(), JavaScriptSyntaxHighlightKeys.JS_GLOBAL_VARIABLE),
            new AttributesDescriptor(JavaScriptLocalize.javascriptGlobalFunction(), JavaScriptSyntaxHighlightKeys.JS_GLOBAL_FUNCTION),
            new AttributesDescriptor(
                JavaScriptLocalize.javascriptInstanceMemberFunction(),
                JavaScriptSyntaxHighlightKeys.JS_INSTANCE_MEMBER_FUNCTION
            ),
            new AttributesDescriptor(
                JavaScriptLocalize.javascriptStaticMemberFunction(),
                JavaScriptSyntaxHighlightKeys.JS_STATIC_MEMBER_FUNCTION
            ),
            new AttributesDescriptor(
                JavaScriptLocalize.javascriptStaticMemberVariable(),
                JavaScriptSyntaxHighlightKeys.JS_STATIC_MEMBER_VARIABLE
            ),
            new AttributesDescriptor(
                JavaScriptLocalize.javascriptInstanceMemberVariable(),
                JavaScriptSyntaxHighlightKeys.JS_INSTANCE_MEMBER_VARIABLE
            ),
            new AttributesDescriptor(JavaScriptLocalize.javascriptMetadata(), JavaScriptSyntaxHighlightKeys.JS_METADATA),
        };
        ADDITIONAL_HIGHLIGHT_DESCRIPTORS.put("local_variable", JavaScriptSyntaxHighlightKeys.JS_LOCAL_VARIABLE);
        ADDITIONAL_HIGHLIGHT_DESCRIPTORS.put("global_variable", JavaScriptSyntaxHighlightKeys.JS_GLOBAL_VARIABLE);
        ADDITIONAL_HIGHLIGHT_DESCRIPTORS.put("instance_variable", JavaScriptSyntaxHighlightKeys.JS_INSTANCE_MEMBER_VARIABLE);
        ADDITIONAL_HIGHLIGHT_DESCRIPTORS.put("instance_method", JavaScriptSyntaxHighlightKeys.JS_INSTANCE_MEMBER_FUNCTION);
        ADDITIONAL_HIGHLIGHT_DESCRIPTORS.put("global_function", JavaScriptSyntaxHighlightKeys.JS_GLOBAL_FUNCTION);
        ADDITIONAL_HIGHLIGHT_DESCRIPTORS.put("parameter", JavaScriptSyntaxHighlightKeys.JS_PARAMETER);
    }


    @Override
    @Nonnull
    public LocalizeValue getDisplayName() {
        return JavaScriptLocalize.javascriptName();
    }

    @Override
    @Nonnull
    public AttributesDescriptor[] getAttributeDescriptors() {
        return ATTRS;
    }

    @Override
    @Nonnull
    public SyntaxHighlighter getHighlighter() {
        return JavaScript15LanguageVersion.getInstance().getSyntaxHighlighter();
    }

    @Override
    @Nonnull
    public String getDemoText() {
        return "var <global_variable>globalVar</global_variable>;\n" +
            "/**\n" +
            " * Constructor for <code>AjaxRequest</code> class\n" +
            " * @param url the url for the request<p/>\n" +
            " */\n" +
            "function <global_function>AjaxRequest</global_function>(<parameter>url</parameter>) {\n" +
            "  var <local_variable>urls</local_variable> = [ \"www.cnn.com\", 5, <global_variable>globalVar</global_variable>];\n" +
            "  this.<instance_variable>request</instance_variable> = new <global_function>XMLHttpRequest</global_function>();\n" +
            "  <parameter>url</parameter> = <parameter>url</parameter>.replace(/^\\s*(.*)/, \"$1\"); // skip leading whitespace\n" +
            "  /* check the url to be in urls */\n" +
            "  var <local_variable>a</local_variable> = \"\\u1111\\z\\n\\u11\";\n" +
            "  this.<instance_method>foo</instance_method> = new function() {};\n" +
            "  <instance_method>foo</instance_method>();\n" +
            "  #\n" +
            "}";
    }

    @Override
    public Map<String, TextAttributesKey> getAdditionalHighlightingTagToDescriptorMap() {
        return ADDITIONAL_HIGHLIGHT_DESCRIPTORS;
    }
}

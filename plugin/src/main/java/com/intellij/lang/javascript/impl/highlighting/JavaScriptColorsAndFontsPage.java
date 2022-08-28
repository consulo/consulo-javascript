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

import consulo.javascript.language.JavaScriptBundle;
import consulo.annotation.component.ExtensionImpl;
import consulo.colorScheme.TextAttributesKey;
import consulo.colorScheme.setting.AttributesDescriptor;
import consulo.colorScheme.setting.ColorDescriptor;
import consulo.javascript.ide.hightlight.JavaScriptSyntaxHighlightKeys;
import consulo.javascript.lang.JavaScript15LanguageVersion;
import consulo.language.editor.colorScheme.setting.ColorSettingsPage;
import consulo.language.editor.highlight.SyntaxHighlighter;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;
import java.util.HashMap;
import java.util.Map;

/**
 * User: Maxim.Mossienko
 * Date: Nov 2, 2005
 * Time: 10:12:13 PM
 */
@ExtensionImpl
public class JavaScriptColorsAndFontsPage implements ColorSettingsPage
{
	private static final AttributesDescriptor[] ATTRS;

	@NonNls
	private static Map<String, TextAttributesKey> ADDITIONAL_HIGHLIGHT_DESCRIPTORS = new HashMap<String, TextAttributesKey>();

	static
	{
		ATTRS = new AttributesDescriptor[]{
				new AttributesDescriptor(JavaScriptBundle.message("javascript.keyword"), JavaScriptSyntaxHighlightKeys.JS_KEYWORD),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.string"), JavaScriptSyntaxHighlightKeys.JS_STRING),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.valid.string.escape"), JavaScriptSyntaxHighlightKeys.JS_VALID_STRING_ESCAPE),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.invalid.string.escape"), JavaScriptSyntaxHighlightKeys.JS_INVALID_STRING_ESCAPE),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.number"), JavaScriptSyntaxHighlightKeys.JS_NUMBER),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.regexp"), JavaScriptSyntaxHighlightKeys.JS_REGEXP),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.linecomment"), JavaScriptSyntaxHighlightKeys.JS_LINE_COMMENT),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.blockcomment"), JavaScriptSyntaxHighlightKeys.JS_BLOCK_COMMENT),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.doccomment"), JavaScriptSyntaxHighlightKeys.JS_DOC_COMMENT),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.operation"), JavaScriptSyntaxHighlightKeys.JS_OPERATION_SIGN),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.parens"), JavaScriptSyntaxHighlightKeys.JS_PARENTHS),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.brackets"), JavaScriptSyntaxHighlightKeys.JS_BRACKETS),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.braces"), JavaScriptSyntaxHighlightKeys.JS_BRACES),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.comma"), JavaScriptSyntaxHighlightKeys.JS_COMMA),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.dot"), JavaScriptSyntaxHighlightKeys.JS_DOT),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.semicolon"), JavaScriptSyntaxHighlightKeys.JS_SEMICOLON),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.badcharacter"), JavaScriptSyntaxHighlightKeys.JS_BAD_CHARACTER),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.docmarkup"), JavaScriptSyntaxHighlightKeys.JS_DOC_MARKUP),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.doctag"), JavaScriptSyntaxHighlightKeys.JS_DOC_TAG),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.parameter"), JavaScriptSyntaxHighlightKeys.JS_PARAMETER),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.local.variable"), JavaScriptSyntaxHighlightKeys.JS_LOCAL_VARIABLE),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.global.variable"), JavaScriptSyntaxHighlightKeys.JS_GLOBAL_VARIABLE),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.global.function"), JavaScriptSyntaxHighlightKeys.JS_GLOBAL_FUNCTION),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.instance.member.function"), JavaScriptSyntaxHighlightKeys.JS_INSTANCE_MEMBER_FUNCTION),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.static.member.function"), JavaScriptSyntaxHighlightKeys.JS_STATIC_MEMBER_FUNCTION),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.static.member.variable"), JavaScriptSyntaxHighlightKeys.JS_STATIC_MEMBER_VARIABLE),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.instance.member.variable"), JavaScriptSyntaxHighlightKeys.JS_INSTANCE_MEMBER_VARIABLE),
				new AttributesDescriptor(JavaScriptBundle.message("javascript.metadata"), JavaScriptSyntaxHighlightKeys.JS_METADATA),
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
	public String getDisplayName()
	{
		return "JavaScript";
	}

	@Override
	@Nonnull
	public AttributesDescriptor[] getAttributeDescriptors()
	{
		return ATTRS;
	}

	@Override
	@Nonnull
	public ColorDescriptor[] getColorDescriptors()
	{
		return ColorDescriptor.EMPTY_ARRAY;
	}

	@Override
	@Nonnull
	public SyntaxHighlighter getHighlighter()
	{
		return JavaScript15LanguageVersion.getInstance().getSyntaxHighlighter();
	}

	@Override
	@Nonnull
	public String getDemoText()
	{
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
	public Map<String, TextAttributesKey> getAdditionalHighlightingTagToDescriptorMap()
	{
		return ADDITIONAL_HIGHLIGHT_DESCRIPTORS;
	}
}

package com.intellij.lang.javascript.highlighting;

import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.options.colors.AttributesDescriptor;
import com.intellij.openapi.options.colors.ColorDescriptor;
import com.intellij.openapi.options.colors.ColorSettingsPage;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by IntelliJ IDEA.
 * User: Maxim.Mossienko
 * Date: Nov 2, 2005
 * Time: 10:12:13 PM
 * To change this template use File | Settings | File Templates.
 */
public class JavaScriptColorsAndFontsPage implements ColorSettingsPage {
  private static final AttributesDescriptor[] ATTRS;

  static {
    ATTRS = new AttributesDescriptor[]{
      new AttributesDescriptor(JSBundle.message("javascript.keyword"), JSHighlighter.JS_KEYWORD),
      new AttributesDescriptor(JSBundle.message("javascript.string"), JSHighlighter.JS_STRING),
      new AttributesDescriptor(JSBundle.message("javascript.valid.string.escape"), JSHighlighter.JS_VALID_STRING_ESCAPE),
      new AttributesDescriptor(JSBundle.message("javascript.invalid.string.escape"), JSHighlighter.JS_INVALID_STRING_ESCAPE),
      new AttributesDescriptor(JSBundle.message("javascript.number"), JSHighlighter.JS_NUMBER),
      new AttributesDescriptor(JSBundle.message("javascript.regexp"), JSHighlighter.JS_REGEXP),
      new AttributesDescriptor(JSBundle.message("javascript.linecomment"), JSHighlighter.JS_LINE_COMMENT),
      new AttributesDescriptor(JSBundle.message("javascript.blockcomment"), JSHighlighter.JS_BLOCK_COMMENT),
      new AttributesDescriptor(JSBundle.message("javascript.doccomment"), JSHighlighter.JS_DOC_COMMENT),
      new AttributesDescriptor(JSBundle.message("javascript.operation"), JSHighlighter.JS_OPERATION_SIGN),
      new AttributesDescriptor(JSBundle.message("javascript.parens"), JSHighlighter.JS_PARENTHS),
      new AttributesDescriptor(JSBundle.message("javascript.brackets"), JSHighlighter.JS_BRACKETS),
      new AttributesDescriptor(JSBundle.message("javascript.braces"), JSHighlighter.JS_BRACES),
      new AttributesDescriptor(JSBundle.message("javascript.comma"), JSHighlighter.JS_COMMA),
      new AttributesDescriptor(JSBundle.message("javascript.dot"), JSHighlighter.JS_DOT),
      new AttributesDescriptor(JSBundle.message("javascript.semicolon"), JSHighlighter.JS_SEMICOLON),
      new AttributesDescriptor(JSBundle.message("javascript.badcharacter"), JSHighlighter.JS_BAD_CHARACTER),
      new AttributesDescriptor(JSBundle.message("javascript.docmarkup"), JSHighlighter.JS_DOC_MARKUP),
      new AttributesDescriptor(JSBundle.message("javascript.doctag"), JSHighlighter.JS_DOC_TAG),
      new AttributesDescriptor(JSBundle.message("javascript.parameter"), JSHighlighter.JS_PARAMETER),
      new AttributesDescriptor(JSBundle.message("javascript.local.variable"), JSHighlighter.JS_LOCAL_VARIABLE),
      new AttributesDescriptor(JSBundle.message("javascript.global.variable"), JSHighlighter.JS_GLOBAL_VARIABLE),
      new AttributesDescriptor(JSBundle.message("javascript.global.function"), JSHighlighter.JS_GLOBAL_FUNCTION),
      new AttributesDescriptor(JSBundle.message("javascript.instance.member.function"), JSHighlighter.JS_INSTANCE_MEMBER_FUNCTION),
      new AttributesDescriptor(JSBundle.message("javascript.static.member.function"), JSHighlighter.JS_STATIC_MEMBER_FUNCTION),
      new AttributesDescriptor(JSBundle.message("javascript.static.member.variable"), JSHighlighter.JS_STATIC_MEMBER_VARIABLE),
      new AttributesDescriptor(JSBundle.message("javascript.instance.member.variable"), JSHighlighter.JS_INSTANCE_MEMBER_VARIABLE),
      new AttributesDescriptor(JSBundle.message("javascript.metadata"), JSHighlighter.JS_METADATA),
    };
  }

  @NotNull
  public String getDisplayName() {
    //noinspection HardCodedStringLiteral
    return "JavaScript";
  }

  public Icon getIcon() {
    return JavaScriptSupportLoader.JAVASCRIPT.getIcon();
  }

  @NotNull
  public AttributesDescriptor[] getAttributeDescriptors() {
    return ATTRS;
  }

  @NotNull
  public ColorDescriptor[] getColorDescriptors() {
    return ColorDescriptor.EMPTY_ARRAY;
  }

  @NotNull
  public SyntaxHighlighter getHighlighter() {
    return SyntaxHighlighter.PROVIDER.create(JavaScriptSupportLoader.JAVASCRIPT, null, null);
  }

  @NotNull
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
           "  #\n"+
           "}";
  }

  private static @NonNls Map<String, TextAttributesKey> ADDITIONAL_HIGHLIGHT_DESCRIPTORS = new HashMap<String, TextAttributesKey>();
    static{
      ADDITIONAL_HIGHLIGHT_DESCRIPTORS.put("local_variable", JSHighlighter.JS_LOCAL_VARIABLE);
      ADDITIONAL_HIGHLIGHT_DESCRIPTORS.put("global_variable", JSHighlighter.JS_GLOBAL_VARIABLE);
      ADDITIONAL_HIGHLIGHT_DESCRIPTORS.put("instance_variable", JSHighlighter.JS_INSTANCE_MEMBER_VARIABLE);
      ADDITIONAL_HIGHLIGHT_DESCRIPTORS.put("instance_method", JSHighlighter.JS_INSTANCE_MEMBER_FUNCTION);
      ADDITIONAL_HIGHLIGHT_DESCRIPTORS.put("global_function", JSHighlighter.JS_GLOBAL_FUNCTION);
      ADDITIONAL_HIGHLIGHT_DESCRIPTORS.put("parameter", JSHighlighter.JS_PARAMETER);
  }

  public Map<String, TextAttributesKey> getAdditionalHighlightingTagToDescriptorMap() {
    return ADDITIONAL_HIGHLIGHT_DESCRIPTORS;
  }
}

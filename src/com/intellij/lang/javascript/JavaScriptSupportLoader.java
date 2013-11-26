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
package com.intellij.lang.javascript;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.lang.DependentLanguage;
import com.intellij.lang.LanguageParserDefinitions;
import com.intellij.lang.LanguageVersion;
import com.intellij.lang.javascript.highlighting.JSHighlighter;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.fileTypes.FileTypeConsumer;
import com.intellij.openapi.fileTypes.FileTypeFactory;
import com.intellij.openapi.fileTypes.FileTypeManager;
import com.intellij.openapi.fileTypes.LanguageFileType;
import com.intellij.openapi.fileTypes.SingleLazyInstanceSyntaxHighlighterFactory;
import com.intellij.openapi.fileTypes.StdFileTypes;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.fileTypes.SyntaxHighlighterFactory;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.xml.XmlFile;
import com.intellij.psi.xml.XmlTag;
import com.intellij.util.ArrayUtil;

/**
 * @by max, maxim.mossienko
 */
public class JavaScriptSupportLoader extends FileTypeFactory {
  public static final LanguageFileType JAVASCRIPT = new JavaScriptFileType();

  public static final JSLanguageDialect ECMA_SCRIPT_L4 = new ECMAL4LanguageDialect();
  public static final JSLanguageDialect JSON = new JSONLanguageDialect();
  public static final JSLanguageDialect GWT_DIALECT = new GwtLanguageDialect();
  public static final JSLanguageDialect JS_IN_HTML_DIALECT = new JsInHtmlLanguageDialect();

  private static final @NonNls String ECMA_SCRIPT_L4_FILE_EXTENSION = "as";
  public static final @NonNls String ECMA_SCRIPT_L4_FILE_EXTENSION2 = "js2";
  private static final @NonNls String ECMA_SCRIPT_L4_FILE_EXTENSION3 = "es";
  public static final @NonNls String JSON_FILE_EXTENSION = "json";
  public static final @NonNls String MXML_FILE_EXTENSION_DOT = ".mxml";
  public static final @NonNls String MXML_FILE_EXTENSION2_DOT = ".mxm";
  public static final @NonNls String MXML_URI = "http://www.adobe.com/2006/mxml";
  public static final @NonNls String MXML_URI2 = "http://www.macromedia.com/2003/mxml";
  public static final @NonNls String MXML_URI3 = "http://ns.adobe.com/mxml/2009";
  public static final @NonNls String MXML_URI4 = "library://ns.adobe.com/flex/spark";
  public static final @NonNls String MXML_URI5 = "library://ns.adobe.com/flex/halo";
  public static final @NonNls String MXML_URI6 = "http://ns.adobe.com/fxg/2008";
  public static final @NonNls String[] MXML_URIS = {MXML_URI, MXML_URI2, MXML_URI3, MXML_URI4, MXML_URI5, MXML_URI6};
  public static final @NonNls String BINDOWS_URI = "http://www.bindows.net";
  @NonNls public static final String ACTION_SCRIPT_CLASS_TEMPLATE_NAME = "ActionScript Class";
  @NonNls public static final String ACTION_SCRIPT_INTERFACE_TEMPLATE_NAME = "ActionScript Interface";
  @NonNls public static final String MXML_COMPONENT_TEMPLATE_NAME = "Mxml Component";

  public static @Nullable JSLanguageDialect getLanguageDialect(VirtualFile file) {
    if (file != null) {
      final String extension = file.getExtension();
      if (ECMA_SCRIPT_L4_FILE_EXTENSION.equals(extension) ||
          ECMA_SCRIPT_L4_FILE_EXTENSION2.equals(extension)  ||
          ECMA_SCRIPT_L4_FILE_EXTENSION3.equals(extension)) {
        return ECMA_SCRIPT_L4;
      } else if (JSON_FILE_EXTENSION.equals(extension)) {
        return JSON;
      } else if (ApplicationManager.getApplication().isUnitTestMode() && GWT_DIALECT.getFileExtension().equals(extension)) {
        return GWT_DIALECT;
      }
    }
    return null;
  }

  public void createFileTypes(final @NotNull FileTypeConsumer consumer) {
    consumer.consume(JAVASCRIPT, "js;" + ECMA_SCRIPT_L4_FILE_EXTENSION + ";" + ECMA_SCRIPT_L4_FILE_EXTENSION2 + ";" + JSON_FILE_EXTENSION +
                                 ";" + ECMA_SCRIPT_L4_FILE_EXTENSION3);
  }

  public static boolean isFlexMxmFile(final PsiFile file) {
    return file.getFileType() == StdFileTypes.XML && nameHasMxmlExtension(file.getName());
  }

  public static boolean isFlexMxmFile(final VirtualFile file) {
    return file.getFileType() == StdFileTypes.XML && nameHasMxmlExtension(file.getName());
  }

  private static boolean nameHasMxmlExtension(final String s) {
    return s.endsWith(MXML_FILE_EXTENSION_DOT) || s.endsWith(MXML_FILE_EXTENSION2_DOT);
  }

  public static boolean isFlexMxmFile(String filename) {
    return FileTypeManager.getInstance().getFileTypeByFileName(filename) == StdFileTypes.XML && nameHasMxmlExtension(filename);
  }

  public static boolean isBindowsFile(final PsiElement element) {
    final PsiFile containingFile = element.getContainingFile();
    final PsiElement tag = element.getParent().getParent();
    if (!(tag instanceof XmlTag)) return false;
    if (BINDOWS_URI.equals(((XmlTag)tag).getNamespace())) return true;
    if (!(containingFile instanceof XmlFile)) return false;

    return "Application".equals(((XmlFile)containingFile).getDocument().getRootTag().getName());
  }

  public static boolean isMxmlNs(final String ns) {
    return ArrayUtil.contains(ns, MXML_URIS);
  }

  private static class JsInHtmlLanguageDialect extends JSLanguageDialect implements DependentLanguage{
    final DialectOptionHolder holder = new DialectOptionHolder(false, false, false);

    public JsInHtmlLanguageDialect() {
      super("JS in HTML");
    }

    {
      SyntaxHighlighterFactory.LANGUAGE_FACTORY.addExplicitExtension(this,
                                                                     new SingleLazyInstanceSyntaxHighlighterFactory() {
          @NotNull
          protected SyntaxHighlighter createHighlighter() {
            return new JSHighlighter(holder);
          }
        }
      );

      LanguageParserDefinitions.INSTANCE.addExplicitExtension(this, new JavascriptParserDefinition() {

        @NotNull
        @Override
        public Lexer createLexer(final Project project, LanguageVersion languageVersion) {
          return new JavaScriptParsingLexer(holder);
        }
      });
    }

    public String getFileExtension() {
      return "jshtml";
    }
  }
}

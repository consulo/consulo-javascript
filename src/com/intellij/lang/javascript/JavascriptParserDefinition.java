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

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.lang.LanguageUtil;
import com.intellij.lang.ParserDefinition;
import com.intellij.lang.PsiParser;
import com.intellij.lang.javascript.parsing.JSParser;
import com.intellij.lang.javascript.psi.impl.*;
import com.intellij.lang.javascript.types.PsiGenerator;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.IFileElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.util.Function;
import org.jetbrains.annotations.NotNull;

/**
 * @author max
 */
public class JavascriptParserDefinition implements ParserDefinition {
  private static Function<ASTNode, PsiElement> ourGwtReferenceExpressionCreator;

  @NotNull
  public Lexer createLexer(Project project) {
    return new JavaScriptParsingLexer(JavascriptLanguage.DIALECT_OPTION_HOLDER);
  }

  public IFileElementType getFileNodeType() {
    return JSElementTypes.FILE;
  }

  @NotNull
  public TokenSet getWhitespaceTokens() {
    return TokenSet.create(JSTokenTypes.WHITE_SPACE);
  }

  @NotNull
  public TokenSet getCommentTokens() {
    return JSTokenTypes.COMMENTS;
  }

  @NotNull
  public TokenSet getStringLiteralElements() {
    return TokenSet.EMPTY;
  }

  @NotNull
  public PsiParser createParser(final Project project) {
    return new JSParser(null);
  }

  public PsiFile createFile(FileViewProvider viewProvider) {
    return new JSFileImpl(viewProvider);
  }

  public SpaceRequirements spaceExistanceTypeBetweenTokens(ASTNode left, ASTNode right) {
    final Lexer lexer = createLexer(left.getPsi().getProject());
    return LanguageUtil.canStickTokensTogetherByLexer(left, right, lexer, 0);
  }

  public static void setGwtReferenceExpressionCreator(final Function<ASTNode, PsiElement> gwtReferenceExpressionCreator) {
    ourGwtReferenceExpressionCreator = gwtReferenceExpressionCreator;
  }

  @NotNull
  public PsiElement createElement(ASTNode node) {
    final IElementType type = node.getElementType();
    if (type instanceof PsiGenerator) {
      final PsiElement element = ((PsiGenerator)type).construct(node);
      if (element != null) return element;
    }

    if (type == JSElementTypes.FUNCTION_DECLARATION) {
      return new JSFunctionImpl(node);
    } else if (type == JSElementTypes.EXTENDS_LIST || type == JSElementTypes.IMPLEMENTS_LIST) {
      return new JSReferenceListImpl(node);
    }
    else if (type == JSElementTypes.FORMAL_PARAMETER) {
      return new JSParameterImpl(node);
    }
    else if (type == JSElementTypes.GWT_REFERENCE_EXPRESSION) {
      return ourGwtReferenceExpressionCreator.fun(node);
    }
    else if (type == JSElementTypes.EMBEDDED_CONTENT) {
      return new JSEmbeddedContentImpl(node);
    } else if (type == JSTokenTypes.XML_JS_SCRIPT ||
               type == JSElementTypes.EMBEDDED_EXPRESSION
              ) {
      return new JSEmbeddedContentImpl(node);
    } else if (type == JSTokenTypes.DOC_COMMENT) {
      return new JSDocCommentImpl(node);
    }

    return new ASTWrapperPsiElement(node);
  }
}

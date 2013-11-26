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
package com.intellij.lang.javascript.parsing;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.openapi.util.Key;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;

/**
 * @author max
 */
public class FunctionParsing extends Parsing {
  static final Key<String> allowEmptyMethodsKey = Key.create("allowEmptyMethodsKey");

  private FunctionParsing() { }  

  public static void parseFunctionExpression(PsiBuilder builder) {
    parseFunction(builder, true);
  }

  public static void parseFunctionDeclaration(PsiBuilder builder) {
    parseFunction(builder, false);
  }

  private static void parseFunction(PsiBuilder builder, boolean expressionContext) {
    parseFunctionNoMarker(builder, expressionContext, builder.mark());
  }

  public static void parseFunctionNoMarker(final PsiBuilder builder,
                                            final boolean expressionContext,
                                            final @NotNull PsiBuilder.Marker functionMarker) {
    if (builder.getTokenType() == JSTokenTypes.FUNCTION_KEYWORD) { // function keyword may be ommited in context of get/set property definition
      builder.advanceLexer();
    }

    // Function name

    final IElementType tokenType = builder.getTokenType();
    if (!expressionContext &&
        ( tokenType == JSTokenTypes.GET_KEYWORD ||
          tokenType == JSTokenTypes.SET_KEYWORD
        )
      ) {
      builder.advanceLexer();
    }

    if (isIdentifierToken(builder.getTokenType())
      ) {
      ExpressionParsing.parseQualifiedTypeName(builder, false);
    }
    else {
      if (!expressionContext && builder.getTokenType() != JSTokenTypes.LPAR /*get/set as name*/) {
        builder.error(JSBundle.message("javascript.parser.message.expected.function.name"));
      }
    }

    parseParameterList(builder);

    ExpressionParsing.tryParseType(builder);

    if (builder.getUserData(allowEmptyMethodsKey) == null) {
      StatementParsing.parseFunctionBody(builder);
    } else {
      if (builder.getTokenType() == JSTokenTypes.SEMICOLON) {
        builder.advanceLexer();
      }

      if (builder.getTokenType() == JSTokenTypes.LBRACE) {
        builder.error(JSBundle.message("interface.function.declaration.should.have.no.body"));
      }
    }

    functionMarker.done(expressionContext ? JSElementTypes.FUNCTION_EXPRESSION : JSElementTypes.FUNCTION_DECLARATION);
  }

  public static void parseAttributeWithoutBrackets(final PsiBuilder builder) {
    final PsiBuilder.Marker attribute = builder.mark();
    if(!checkMatches(builder, JSTokenTypes.IDENTIFIER, JSBundle.message("javascript.parser.message.expected.identifier"))) {
      attribute.drop();
      return;
    }
    parseAttributeBody(builder);
    attribute.done(JSElementTypes.ATTRIBUTE);
  }

  static void parseAttributesList(final PsiBuilder builder) {
    if (!isECMAL4(builder)) return;
    final PsiBuilder.Marker modifierList = builder.mark();

    boolean seenNs = false;
    boolean seenAnyAttributes = false;

    try {
      boolean hasSomethingInAttrList = true;

      while(hasSomethingInAttrList) {
        hasSomethingInAttrList = false;

        while (builder.getTokenType() == JSTokenTypes.LBRACKET) {
          seenAnyAttributes = true;
          PsiBuilder.Marker attribute = builder.mark();

          builder.advanceLexer();

          if (builder.eof() ||
              ( !checkMatches(builder, JSTokenTypes.IDENTIFIER, JSBundle.message("javascript.parser.message.expected.identifier")) &&
                builder.getTokenType() != JSTokenTypes.RBRACKET)) {
            attribute.drop();
            return;
          }

          while(builder.getTokenType() != JSTokenTypes.RBRACKET) {
            parseAttributeBody(builder);

            if (builder.eof()) {
              attribute.done(JSElementTypes.ATTRIBUTE);
              builder.error(JSBundle.message("javascript.parser.message.expected.rbracket"));
              return;
            }
          }

          builder.advanceLexer();
          attribute.done(JSElementTypes.ATTRIBUTE);
          hasSomethingInAttrList = true;
        }

        if (builder.getTokenType() == JSTokenTypes.INCLUDE_KEYWORD) {
          hasSomethingInAttrList = true;
          StatementParsing.parseIncludeDirective(builder);
        }

        if (builder.getTokenType() == JSTokenTypes.IDENTIFIER && !seenNs) {
          hasSomethingInAttrList = true;
          seenNs = true;
          PsiBuilder.Marker marker = builder.mark();
          builder.advanceLexer();
          marker.done(JSElementTypes.REFERENCE_EXPRESSION);
        }

        while(JSTokenTypes.MODIFIERS.contains(builder.getTokenType())) {
          seenAnyAttributes = true;
          hasSomethingInAttrList = true;
          if (builder.getTokenType() == JSTokenTypes.NATIVE_KEYWORD) builder.putUserData(allowEmptyMethodsKey, "");
          builder.advanceLexer();
        }

        if (builder.eof()) {
          return;
        }
      }
    }
    finally {
      final IElementType currentTokenType = builder.getTokenType();
      
      if (seenNs &&
          !seenAnyAttributes &&
          ( currentTokenType != JSTokenTypes.VAR_KEYWORD &&
            currentTokenType != JSTokenTypes.FUNCTION_KEYWORD &&
            currentTokenType != JSTokenTypes.CLASS_KEYWORD &&
            currentTokenType != JSTokenTypes.INTERFACE_KEYWORD
          )
         ) {
        modifierList.drop();
      } else {
        modifierList.done(JSElementTypes.ATTRIBUTE_LIST);
      }
    }
  }

  private static void parseAttributeBody(final PsiBuilder builder) {
    final boolean haveLParen = checkMatches(builder, JSTokenTypes.LPAR, JSBundle.message("javascript.parser.message.expected.lparen"));
    boolean hasName;

    while(haveLParen) {
      PsiBuilder.Marker attributeNameValuePair;
      hasName = builder.getTokenType() == JSTokenTypes.IDENTIFIER;

      if (builder.getTokenType() == JSTokenTypes.COMMA) {
        builder.error(JSBundle.message("javascript.parser.message.expected.identifer.or.value"));
        break;
      }
      if (builder.getTokenType() == JSTokenTypes.RBRACKET) break;

      attributeNameValuePair = builder.mark();
      builder.advanceLexer();

      if (hasName && builder.getTokenType() != JSTokenTypes.COMMA && builder.getTokenType() != JSTokenTypes.RPAR) {
        checkMatches(builder, JSTokenTypes.EQ, JSBundle.message("javascript.parser.message.expected.equal"));

        if (builder.getTokenType() != JSTokenTypes.COMMA && builder.getTokenType() != JSTokenTypes.RBRACKET && builder.getTokenType() != JSTokenTypes.RPAR) {
          builder.advanceLexer();
        } else {
          builder.error(JSBundle.message("javascript.parser.message.expected.value"));
        }
      }

      if (attributeNameValuePair != null) attributeNameValuePair.done(JSElementTypes.ATTRIBUTE_NAME_VALUE_PAIR);
      if (builder.getTokenType() != JSTokenTypes.COMMA) break;
      builder.advanceLexer();

      if (builder.eof()) {
        builder.error(JSBundle.message("javascript.parser.message.expected.rparen"));
        return;
      }
    }

    if (haveLParen) checkMatches(builder, JSTokenTypes.RPAR, JSBundle.message("javascript.parser.message.expected.rparen"));
    else builder.advanceLexer();
  }

  private static void parseParameterList(final PsiBuilder builder) {
    final PsiBuilder.Marker parameterList;
    if (builder.getTokenType() != JSTokenTypes.LPAR) {
      builder.error(JSBundle.message("javascript.parser.message.expected.lparen"));
      parameterList = builder.mark(); // To have non-empty parameters list at all the time.
      parameterList.done(JSElementTypes.PARAMETER_LIST);
      return;
    }
    else {
      parameterList = builder.mark();
      builder.advanceLexer();
    }

    boolean first = true;
    while (builder.getTokenType() != JSTokenTypes.RPAR) {
      if (first) {
        first = false;
      }
      else {
        if (builder.getTokenType() == JSTokenTypes.COMMA) {
          builder.advanceLexer();
        }
        else {
          builder.error(JSBundle.message("javascript.parser.message.expected.comma.or.rparen"));
          break;
        }
      }

      final PsiBuilder.Marker parameter = builder.mark();
      if (builder.getTokenType() == JSTokenTypes.DOT_DOT_DOT) {
        builder.advanceLexer();
      }
      if (JSTokenTypes.IDENTIFIER_TOKENS_SET.contains(builder.getTokenType())) {
        builder.advanceLexer();
        ExpressionParsing.tryParseType(builder);
        if (builder.getTokenType() == JSTokenTypes.EQ) {
          builder.advanceLexer();
          ExpressionParsing.parseSimpleExpression(builder);
        }
        parameter.done(JSElementTypes.FORMAL_PARAMETER);
      }
      else {
        builder.error(JSBundle.message("javascript.parser.message.expected.formal.parameter.name"));
        parameter.drop();
      }
    }

    if (builder.getTokenType() == JSTokenTypes.RPAR) {
      builder.advanceLexer();
    }

    parameterList.done(JSElementTypes.PARAMETER_LIST);
  }
}

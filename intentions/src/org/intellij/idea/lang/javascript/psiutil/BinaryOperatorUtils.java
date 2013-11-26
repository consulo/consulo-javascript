/*
 * Copyright 2005-2006 Olivier Descout
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
package org.intellij.idea.lang.javascript.psiutil;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NonNls;

import java.util.HashMap;
import java.util.Map;

public class BinaryOperatorUtils {
    private static final Map<IElementType, OperatorProperties> operators;

    private BinaryOperatorUtils() {}

    public static boolean isBinaryOperator(JSExpression expression) {
        return (expression instanceof JSBinaryExpression &&
                operators.containsKey(((JSBinaryExpression) expression).getOperationSign()));
    }

    public static boolean isShiftOperator(JSExpression expression) {
        if (expression instanceof JSBinaryExpression) {
            final IElementType sign = ((JSBinaryExpression) expression).getOperationSign();

            return (sign.equals(JSTokenTypes.LTLT) ||
                    sign.equals(JSTokenTypes.GTGT) ||
                    sign.equals(JSTokenTypes.GTGTGT));
        } else {
            return false;
        }
    }

    public static String getOperatorText(IElementType operator) {
      if (operator == null) return "";
      final OperatorProperties operatorProperties = operators.get(operator);
      assert operatorProperties != null:"Operator properties is null for " + operator;
      return operatorProperties.text;
    }

    public static boolean isCommutative(IElementType operator) {
      if (operator == null) return false;
      final OperatorProperties operatorProperties = operators.get(operator);
      assert operatorProperties != null:"Operator properties is null for " + operator;
      return operatorProperties.commutative;
    }

    static {
        operators = new HashMap<IElementType, OperatorProperties>(39);

        operators.put(JSTokenTypes.LT         , new OperatorProperties("<"   , false));
        operators.put(JSTokenTypes.GT         , new OperatorProperties(">"   , false));
        operators.put(JSTokenTypes.LE         , new OperatorProperties("<="  , false));
        operators.put(JSTokenTypes.GE         , new OperatorProperties(">="  , false));
        operators.put(JSTokenTypes.EQEQ       , new OperatorProperties("=="  , true));
        operators.put(JSTokenTypes.NE         , new OperatorProperties("!="  , true));
        operators.put(JSTokenTypes.EQEQEQ     , new OperatorProperties("===" , true));
        operators.put(JSTokenTypes.NEQEQ      , new OperatorProperties("!==" , true));
        operators.put(JSTokenTypes.PLUS       , new OperatorProperties("+"   , true));
        operators.put(JSTokenTypes.MINUS      , new OperatorProperties("-"   , false));
        operators.put(JSTokenTypes.MULT       , new OperatorProperties("*"   , true));
        operators.put(JSTokenTypes.DIV        , new OperatorProperties("/"   , false));
        operators.put(JSTokenTypes.PERC       , new OperatorProperties("%"   , false));
        operators.put(JSTokenTypes.PLUSPLUS   , new OperatorProperties("++"  , false));
        operators.put(JSTokenTypes.MINUSMINUS , new OperatorProperties("--"  , false));
        operators.put(JSTokenTypes.LTLT       , new OperatorProperties("<<"  , false));
        operators.put(JSTokenTypes.GTGT       , new OperatorProperties(">>"  , false));
        operators.put(JSTokenTypes.GTGTGT     , new OperatorProperties(">>>" , false));
        operators.put(JSTokenTypes.AND        , new OperatorProperties("&"   , true));
        operators.put(JSTokenTypes.OR         , new OperatorProperties("|"   , true));
        operators.put(JSTokenTypes.XOR        , new OperatorProperties("^"   , true));
        operators.put(JSTokenTypes.EXCL       , new OperatorProperties("!"   , false));
        operators.put(JSTokenTypes.TILDE      , new OperatorProperties("~"   , false));
        operators.put(JSTokenTypes.ANDAND     , new OperatorProperties("&&"  , true));
        operators.put(JSTokenTypes.OROR       , new OperatorProperties("||"  , true));
        operators.put(JSTokenTypes.EQ         , new OperatorProperties("="   , false));
        operators.put(JSTokenTypes.PLUSEQ     , new OperatorProperties("+="  , false));
        operators.put(JSTokenTypes.MINUSEQ    , new OperatorProperties("-="  , false));
        operators.put(JSTokenTypes.MULTEQ     , new OperatorProperties("*="  , false));
        operators.put(JSTokenTypes.PERCEQ     , new OperatorProperties("%="  , false));
        operators.put(JSTokenTypes.LTLTEQ     , new OperatorProperties("<<=" , false));
        operators.put(JSTokenTypes.GTGTEQ     , new OperatorProperties(">>=" , false));
        operators.put(JSTokenTypes.GTGTGTEQ   , new OperatorProperties(">>>=", false));
        operators.put(JSTokenTypes.ANDEQ      , new OperatorProperties("&="  , false));
        operators.put(JSTokenTypes.OREQ       , new OperatorProperties("|="  , false));
        operators.put(JSTokenTypes.XOREQ      , new OperatorProperties("^="  , false));
        operators.put(JSTokenTypes.DIVEQ      , new OperatorProperties("/="  , false));
        operators.put(JSTokenTypes.COMMA      , new OperatorProperties(","  , false));
        operators.put(JSTokenTypes.IS_KEYWORD, new OperatorProperties("is"  , false));
        operators.put(JSTokenTypes.AS_KEYWORD, new OperatorProperties("as"  , false));
        operators.put(JSTokenTypes.DELETE_KEYWORD, new OperatorProperties("delete "  , false));
        operators.put(JSTokenTypes.VOID_KEYWORD, new OperatorProperties("void "  , false));
        operators.put(JSTokenTypes.TYPEOF_KEYWORD, new OperatorProperties("typeof "  , false));
    }

    private static class OperatorProperties {
        String  text;
        boolean commutative;

        public OperatorProperties(@NonNls String text, boolean commutative) {
            this.text        = text;
            this.commutative = commutative;
        }
    }
}

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
import consulo.language.ast.IElementType;

import java.util.HashMap;
import java.util.Map;

public class ComparisonUtils {
    private static final Map<IElementType, OperatorTexts> operators;

    private ComparisonUtils() {
    }

    public static boolean isComparisonOperator(JSExpression expression) {
        return expression instanceof JSBinaryExpression binaryExpression
            && operators.containsKey(binaryExpression.getOperationSign());
    }

    public static String getOperatorText(IElementType operator) {
        return operators.get(operator).getText();
    }

    public static boolean isEqualityTestExpression(JSBinaryExpression expression) {
        IElementType operator = expression.getOperationSign();
        return JSTokenTypes.EQEQ.equals(operator) || JSTokenTypes.NE.equals(operator);
    }

    public static boolean mayBeEqualExpression(JSBinaryExpression expression) {
        IElementType operator = expression.getOperationSign();
        return JSTokenTypes.EQEQ.equals(operator) || JSTokenTypes.EQEQEQ.equals(operator)
            || JSTokenTypes.LE.equals(operator) || JSTokenTypes.GE.equals(operator);
    }

    public static String getNegatedOperatorText(IElementType operator) {
        return operators.get(operator).getNegatedText();
    }

    public static String getFlippedOperatorText(IElementType operator) {
        return operators.get(operator).getFlippedText();
    }

    static {
        operators = new HashMap<IElementType, OperatorTexts>(8);

        operators.put(JSTokenTypes.EQEQ, new OperatorTexts("==", "!=", "=="));
        operators.put(JSTokenTypes.EQEQEQ, new OperatorTexts("===", "!==", "==="));
        operators.put(JSTokenTypes.NE, new OperatorTexts("!=", "==", "!="));
        operators.put(JSTokenTypes.NEQEQ, new OperatorTexts("!==", "===", "!=="));
        operators.put(JSTokenTypes.GT, new OperatorTexts(">", "<=", "<"));
        operators.put(JSTokenTypes.LT, new OperatorTexts("<", ">=", ">"));
        operators.put(JSTokenTypes.GE, new OperatorTexts(">=", "<", "<="));
        operators.put(JSTokenTypes.LE, new OperatorTexts("<=", ">", ">="));
    }

    private static class OperatorTexts {
        private String text;
        private String negatedText;
        private String flippedText;

        public OperatorTexts(String text, String negatedText, String flippedText) {
            this.text = text;
            this.negatedText = negatedText;
            this.flippedText = flippedText;
        }

        public String getText() {
            return this.text;
        }

        public String getNegatedText() {
            return this.negatedText;
        }

        public String getFlippedText() {
            return this.flippedText;
        }
    }
}

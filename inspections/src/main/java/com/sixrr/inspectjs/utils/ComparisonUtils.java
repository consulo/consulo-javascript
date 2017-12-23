package com.sixrr.inspectjs.utils;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class ComparisonUtils {
    private ComparisonUtils() {
        super();
    }

    private static final Map<IElementType, String> s_invertedComparisons = new HashMap<IElementType, String>(6);
    private static final Set<IElementType> s_comparisonStrings = new HashSet<IElementType>(6);
    private static final Map<IElementType, String> s_swappedComparisons = new HashMap<IElementType, String>(6);

    static {
        s_comparisonStrings.add(JSTokenTypes.EQEQ);
        s_comparisonStrings.add(JSTokenTypes.EQEQEQ);
        s_comparisonStrings.add(JSTokenTypes.NE);
        s_comparisonStrings.add(JSTokenTypes.NEQEQ);
        s_comparisonStrings.add(JSTokenTypes.LT);
        s_comparisonStrings.add(JSTokenTypes.GT);
        s_comparisonStrings.add(JSTokenTypes.GE);
        s_comparisonStrings.add(JSTokenTypes.LE);

        s_swappedComparisons.put(JSTokenTypes.EQEQ, "==");
        s_swappedComparisons.put(JSTokenTypes.EQEQEQ, "===");
        s_swappedComparisons.put(JSTokenTypes.NE, "!=");
        s_swappedComparisons.put(JSTokenTypes.NEQEQ, "!==");
        s_swappedComparisons.put(JSTokenTypes.GT, "<");
        s_swappedComparisons.put(JSTokenTypes.LT, ">");
        s_swappedComparisons.put(JSTokenTypes.GE, "<=");
        s_swappedComparisons.put(JSTokenTypes.LE, ">=");

        s_invertedComparisons.put(JSTokenTypes.EQEQ, "!=");
        s_invertedComparisons.put(JSTokenTypes.EQEQEQ, "!==");
        s_invertedComparisons.put(JSTokenTypes.NE, "==");
        s_invertedComparisons.put(JSTokenTypes.NEQEQ, "===");
        s_invertedComparisons.put(JSTokenTypes.GT, "<=");
        s_invertedComparisons.put(JSTokenTypes.LT, ">=");
        s_invertedComparisons.put(JSTokenTypes.GE, "<");
        s_invertedComparisons.put(JSTokenTypes.LE, ">");
    }

    public static boolean isComparison(@Nullable JSExpression exp) {
        if (!(exp instanceof JSBinaryExpression)) {
            return false;
        }
        final JSBinaryExpression binaryExpression = (JSBinaryExpression) exp;
        final IElementType sign = binaryExpression.getOperationSign();
        return s_comparisonStrings.contains(sign);
    }

    public static String getFlippedComparison(@NotNull IElementType str) {
        return s_swappedComparisons.get(str);
    }

    public static boolean isEqualityComparison(@NotNull JSBinaryExpression operator) {
        final IElementType sign = operator.getOperationSign();
        return JSTokenTypes.EQEQ.equals(sign) || JSTokenTypes.NE.equals(sign);
    }

    public static String getNegatedComparison(@NotNull IElementType str) {
        return s_invertedComparisons.get(str);
    }
}

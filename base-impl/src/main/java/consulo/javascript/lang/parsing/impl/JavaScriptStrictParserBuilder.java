package consulo.javascript.lang.parsing.impl;

import com.intellij.lang.javascript.JSTokenTypes;
import consulo.javascript.language.JavaScriptConstants;
import consulo.javascript.lang.JavaScriptTokenSets;
import consulo.javascript.lang.parsing.JavaScriptParserBuilder;
import consulo.language.ast.IElementType;
import consulo.language.parser.PsiBuilder;
import consulo.util.collection.ArrayUtil;
import consulo.util.lang.StringUtil;
import consulo.util.lang.ThreeState;

import jakarta.annotation.Nullable;

import java.util.HashMap;
import java.util.Map;

/**
 * @author VISTALL
 * @since 15.02.2016
 */
public class JavaScriptStrictParserBuilder extends JavaScriptParserBuilder {
    private static final int ourExpectedStringSize = JavaScriptConstants.USE_STRICT.length() + 2; // '' or ""

    private ThreeState myStrictState = ThreeState.UNSURE;

    private Map<IElementType, ThreeState> myStrictKeywords = new HashMap<>();

    private int[] myDisableNonStrictRemap = ArrayUtil.EMPTY_INT_ARRAY;

    public JavaScriptStrictParserBuilder(PsiBuilder delegate) {
        super(delegate);
    }

    public void onlyInStrictMode(IElementType elementType) {
        myStrictKeywords.put(elementType, ThreeState.YES);
    }

    public void disableNonStrictRemap(int offset) {
        myDisableNonStrictRemap = ArrayUtil.append(myDisableNonStrictRemap, offset);
    }

    @Nullable
    @Override
    public IElementType getTokenType() {
        checkStrictMode();
        IElementType tokenType = super.getTokenType();
        ThreeState threeState = myStrictKeywords.get(tokenType);
        if (threeState != null) {
            if (threeState == myStrictState) {
                return tokenType;
            }
            else {
                int currentOffset = getCurrentOffset();
                if (ArrayUtil.indexOf(myDisableNonStrictRemap, currentOffset) != -1) {
                    return tokenType;
                }

                return JSTokenTypes.IDENTIFIER;
            }
        }
        return tokenType;
    }

    @Override
    public void advanceLexer() {
        IElementType tokenType = getTokenType();

        IElementType originalTokenType = super.getTokenType();

        if (tokenType != originalTokenType) {
            remapCurrentToken(tokenType);
        }

        super.advanceLexer();
    }

    public boolean isStrictMode() {
        return myStrictState == ThreeState.YES;
    }

    private void checkStrictMode() {
        if (myStrictState != ThreeState.UNSURE) {
            return;
        }

        IElementType tokenType = super.getTokenType();
        if (JavaScriptTokenSets.STRING_LITERALS.contains(tokenType)) {
            String tokenText = getTokenText();
            if (tokenText == null) {
                return;
            }

            int length = tokenText.length();
            if (ourExpectedStringSize == length) {
                String stringContent = StringUtil.unquoteString(tokenText);
                if (JavaScriptConstants.USE_STRICT.equals(stringContent)) {
                    myStrictState = ThreeState.YES;
                    return;
                }
            }
        }
        myStrictState = ThreeState.NO;
    }
}

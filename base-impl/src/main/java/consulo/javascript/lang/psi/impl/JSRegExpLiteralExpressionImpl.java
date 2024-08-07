package consulo.javascript.lang.psi.impl;

import com.intellij.lang.javascript.psi.impl.JSLiteralExpressionImpl;
import consulo.language.ast.ASTNode;

/**
 * Own implementation for RegExp literal for correct registration RegExpLanguageHost.
 * <p>
 * String literal with regexp will have own RegExp host
 *
 * @author VISTALL
 * @since 21/12/2021
 */
public class JSRegExpLiteralExpressionImpl extends JSLiteralExpressionImpl {
    public JSRegExpLiteralExpressionImpl(ASTNode node) {
        super(node);
    }
}

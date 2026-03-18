package consulo.typescript.language.impl;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.impl.JSDocCommentImpl;
import com.intellij.lang.javascript.psi.impl.JSFileImpl;
import com.intellij.lang.javascript.types.JSFileElementType;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.Language;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.ast.IFileElementType;
import consulo.language.file.FileViewProvider;
import consulo.language.lexer.Lexer;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.util.LanguageUtil;
import consulo.language.version.LanguageVersionableParserDefinition;
import consulo.typescript.language.TypeScriptLanguage;
import consulo.typescript.language.impl.psi.TypeScriptTypesFactory;

/**
 * Parser definition for the TypeScript language.
 * Registered for {@link TypeScriptLanguage} — no version checks needed.
 *
 * @author VISTALL
 * @since 2026-03-18
 */
@ExtensionImpl
public class TypeScriptParserDefinition extends LanguageVersionableParserDefinition {
    private static final IFileElementType FILE = new JSFileElementType(TypeScriptLanguage.INSTANCE);

    @Override
    public Language getLanguage() {
        return TypeScriptLanguage.INSTANCE;
    }

    @Override
    public IFileElementType getFileNodeType() {
        return FILE;
    }

    @Override
    public PsiElement createElement(ASTNode node) {
        IElementType type = node.getElementType();

        if (type == JSTokenTypes.DOC_COMMENT) {
            return new JSDocCommentImpl(node);
        }

        return TypeScriptTypesFactory.createElement(node);
    }

    @Override
    public PsiFile createFile(FileViewProvider fileViewProvider) {
        return new JSFileImpl(fileViewProvider);
    }

    @Override
    public SpaceRequirements spaceExistanceTypeBetweenTokens(ASTNode left, ASTNode right) {
        PsiElement leftPsi = left.getPsi();
        Lexer lexer = createLexer(leftPsi.getLanguageVersion());
        return LanguageUtil.canStickTokensTogetherByLexer(left, right, lexer);
    }
}

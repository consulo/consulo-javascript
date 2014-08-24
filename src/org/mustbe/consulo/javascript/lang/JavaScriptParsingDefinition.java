package org.mustbe.consulo.javascript.lang;

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.ASTNode;
import com.intellij.lang.LanguageUtil;
import com.intellij.lang.LanguageVersionableParserDefinition;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.impl.JSFileImpl;
import com.intellij.lexer.Lexer;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IFileElementType;

/**
 * @author VISTALL
 * @since 24.08.14
 */
public class JavaScriptParsingDefinition extends LanguageVersionableParserDefinition
{
	@NotNull
	@Override
	public IFileElementType getFileNodeType()
	{
		return JSElementTypes.FILE;
	}

	@NotNull
	@Override
	public PsiElement createElement(ASTNode astNode)
	{
		return null;
	}

	@Override
	public PsiFile createFile(FileViewProvider fileViewProvider)
	{
		return new JSFileImpl(fileViewProvider);
	}

	@NotNull
	@Override
	public SpaceRequirements spaceExistanceTypeBetweenTokens(ASTNode left, ASTNode right)
	{
		PsiElement leftPsi = left.getPsi();
		final Lexer lexer = createLexer(leftPsi.getProject(), leftPsi.getLanguageVersion());
		return LanguageUtil.canStickTokensTogetherByLexer(left, right, lexer);
	}
}

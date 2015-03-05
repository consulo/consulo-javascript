package org.mustbe.consulo.javascript.lang;

import org.jetbrains.annotations.NotNull;
import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.lang.LanguageUtil;
import com.intellij.lang.LanguageVersionableParserDefinition;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.impl.JSDocCommentImpl;
import com.intellij.lang.javascript.psi.impl.JSEmbeddedContentImpl;
import com.intellij.lang.javascript.psi.impl.JSFileImpl;
import com.intellij.lang.javascript.psi.impl.JSFunctionImpl;
import com.intellij.lang.javascript.psi.impl.JSParameterImpl;
import com.intellij.lang.javascript.psi.impl.JSReferenceListImpl;
import com.intellij.lang.javascript.types.PsiGenerator;
import com.intellij.lexer.Lexer;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IElementType;
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
	public PsiElement createElement(ASTNode node)
	{
		final IElementType type = node.getElementType();
		if(type instanceof PsiGenerator)
		{
			final PsiElement element = ((PsiGenerator) type).construct(node);
			if(element != null)
			{
				return element;
			}
		}

		if(type == JSElementTypes.FUNCTION_DECLARATION)
		{
			return new JSFunctionImpl(node);
		}
		else if(type == JSElementTypes.EXTENDS_LIST || type == JSElementTypes.IMPLEMENTS_LIST)
		{
			return new JSReferenceListImpl(node);
		}
		else if(type == JSElementTypes.FORMAL_PARAMETER)
		{
			return new JSParameterImpl(node);
		}
		else if(type == JSElementTypes.EMBEDDED_CONTENT)
		{
			return new JSEmbeddedContentImpl(node);
		}
		else if(type == JSTokenTypes.XML_JS_SCRIPT || type == JSElementTypes.EMBEDDED_EXPRESSION)
		{
			return new JSEmbeddedContentImpl(node);
		}
		else if(type == JSTokenTypes.DOC_COMMENT)
		{
			return new JSDocCommentImpl(node);
		}

		return new ASTWrapperPsiElement(node);
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
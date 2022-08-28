package consulo.javascript.jsx.language.psi.impl;

import com.intellij.lang.javascript.psi.impl.JSXmlLiteralExpressionImpl;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.pattern.StandardPatterns;
import consulo.language.psi.*;
import consulo.language.util.ProcessingContext;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2019-12-17
 */
@ExtensionImpl
public class JSXmlTagReferenceContributor extends PsiReferenceContributor
{
	@Override
	public void registerReferenceProviders(PsiReferenceRegistrar registrar)
	{
		registrar.registerReferenceProvider(StandardPatterns.psiElement(JSXmlLiteralExpressionImpl.class), new PsiReferenceProvider()
		{
			@Nonnull
			@Override
			public PsiReference[] getReferencesByElement(@Nonnull PsiElement psiElement, @Nonnull ProcessingContext processingContext)
			{
				return PsiReference.EMPTY_ARRAY;
				// TODO 
//				ASTNode[] children = psiElement.getNode().getChildren(TokenSet.create(JSTokenTypes.XML_NAME));
//				if(children.length == 0)
//				{
//					return PsiReference.EMPTY_ARRAY;
//				}
//				return Arrays.stream(children).map(node -> new JSXmlTagReference(node.getPsi())).toArray(PsiReference[]::new);
			}
		});
	}

	@Nonnull
	@Override
	public Language getLanguage()
	{
		return JavaScriptLanguage.INSTANCE;
	}
}

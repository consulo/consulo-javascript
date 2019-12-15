package consulo.javascript.lang.psi.impl.resolve;

import com.intellij.lang.javascript.psi.impl.JSReferenceExpressionImpl;
import com.intellij.lang.javascript.psi.resolve.JSImportedElementResolveResult;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.ResolveState;
import consulo.annotation.access.RequiredReadAction;
import consulo.lang.LanguageVersion;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * @author VISTALL
 * @since 2019-12-14
 */
public abstract class ResolveHelper
{
	private static final ResolveHelper DEFAULT = new ResolveHelper()
	{
	};

	public static ResolveHelper find(PsiElement element)
	{
		LanguageVersion languageVersion = element.getLanguageVersion();
		if(languageVersion instanceof JavaScriptVersionWithHelper)
		{
			return ((JavaScriptVersionWithHelper) languageVersion).getHelper();
		}

		return DEFAULT;
	}

	@Nullable
	public JSImportedElementResolveResult resolveTypeNameUsingImports(final @Nonnull String referencedName, PsiNamedElement parent)
	{
		return null;
	}

	public boolean execute(ResolveProcessor resolveProcessor, PsiElement element, ResolveState state)
	{
		return false;
	}

	@RequiredReadAction
	public boolean isResolveTo(JSReferenceExpressionImpl expression, PsiElement element)
	{
		return false;
	}
}

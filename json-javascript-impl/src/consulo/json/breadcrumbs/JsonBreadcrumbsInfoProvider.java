package consulo.json.breadcrumbs;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.lang.Language;
import com.intellij.lang.javascript.psi.JSProperty;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.xml.breadcrumbs.BreadcrumbsInfoProvider;
import consulo.annotations.RequiredReadAction;
import consulo.javascript.lang.JavaScriptLanguage;
import consulo.json.JsonFileType;

/**
 * @author VISTALL
 * @since 07.12.2015
 */
public class JsonBreadcrumbsInfoProvider extends BreadcrumbsInfoProvider
{
	@NotNull
	@Override
	public Language getLanguage()
	{
		return JavaScriptLanguage.INSTANCE;
	}

	@Override
	public boolean validateFileProvider(@NotNull FileViewProvider fileViewProvider)
	{
		return fileViewProvider.getFileType() == JsonFileType.INSTANCE;
	}

	@RequiredReadAction
	@Override
	public boolean acceptElement(@NotNull PsiElement psiElement)
	{
		return psiElement instanceof JSProperty && ((JSProperty) psiElement).getName() != null;
	}

	@RequiredReadAction
	@NotNull
	@Override
	public String getElementInfo(@NotNull PsiElement psiElement)
	{
		if(psiElement instanceof JSProperty)
		{
			return ((JSProperty) psiElement).getName();
		}
		throw new IllegalArgumentException(psiElement.getClass().getName() + " is not supported");
	}

	@RequiredReadAction
	@Nullable
	@Override
	public String getElementTooltip(@NotNull PsiElement psiElement)
	{
		return null;
	}
}

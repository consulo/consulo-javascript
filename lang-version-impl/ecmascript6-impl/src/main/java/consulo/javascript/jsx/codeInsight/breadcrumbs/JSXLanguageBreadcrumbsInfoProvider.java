package consulo.javascript.jsx.codeInsight.breadcrumbs;

import com.intellij.lang.Language;
import com.intellij.xml.breadcrumbs.XmlLanguageBreadcrumbsInfoProvider;
import consulo.javascript.lang.JavaScriptLanguage;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2019-12-15
 */
public class JSXLanguageBreadcrumbsInfoProvider extends XmlLanguageBreadcrumbsInfoProvider
{
	@Nonnull
	@Override
	public Language getLanguage()
	{
		return JavaScriptLanguage.INSTANCE;
	}
}

package consulo.javascript.lang;

import com.intellij.openapi.util.text.StringUtil;
import consulo.lang.LanguageVersion;
import jakarta.inject.Singleton;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;

/**
 * @author VISTALL
 * @since 23-Dec-17
 */
@Singleton
public class StandardJavaScriptVersionsImpl extends StandardJavaScriptVersions
{
	@Nonnull
	public BaseJavaScriptLanguageVersion getDefaultVersion()
	{
		return JavaScript15LanguageVersion.getInstance();
	}

	@Nonnull
	public List<JavaScriptLanguageVersion> getValidLanguageVersions()
	{
		List<JavaScriptLanguageVersion> list = new ArrayList<>();
		LanguageVersion[] versions = JavaScriptLanguage.INSTANCE.getVersions();
		for(LanguageVersion version : versions)
		{
			if(version instanceof StandardJavaScriptVersions.Marker)
			{
				list.add((BaseJavaScriptLanguageVersion) version);
			}
		}

		list.sort((o1, o2) -> StringUtil.naturalCompare(o1.getPresentableName(), o2.getPresentableName()));
		return list;
	}

	@Nonnull
	public BaseJavaScriptLanguageVersion findVersionById(@Nullable String id)
	{
		if(StringUtil.isEmpty(id))
		{
			return getDefaultVersion();
		}

		LanguageVersion[] versions = JavaScriptLanguage.INSTANCE.getVersions();
		for(LanguageVersion version : versions)
		{
			if(version instanceof StandardJavaScriptVersions.Marker && id.equals(version.getId()))
			{
				return (BaseJavaScriptLanguageVersion) version;
			}
		}
		return getDefaultVersion();
	}
}

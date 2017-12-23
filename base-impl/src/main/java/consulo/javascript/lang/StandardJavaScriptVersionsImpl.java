package consulo.javascript.lang;

import java.util.ArrayList;
import java.util.List;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.containers.ContainerUtil;
import consulo.lang.LanguageVersion;

/**
 * @author VISTALL
 * @since 23-Dec-17
 */
public class StandardJavaScriptVersionsImpl extends StandardJavaScriptVersions
{
	@NotNull
	public BaseJavaScriptLanguageVersion getDefaultVersion()
	{
		return JavaScript15LanguageVersion.getInstance();
	}

	@NotNull
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

		ContainerUtil.sort(list, (o1, o2) -> ((Marker) o1).getWeight() - ((Marker) o2).getWeight());
		return list;
	}

	@NotNull
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

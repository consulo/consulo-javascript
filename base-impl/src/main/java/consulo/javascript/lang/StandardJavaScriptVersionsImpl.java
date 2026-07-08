package consulo.javascript.lang;

import consulo.annotation.component.ServiceImpl;
import consulo.javascript.internal.DefaultJavaScriptVersion;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.javascript.language.JavaScriptLanguageVersion;
import consulo.javascript.language.StandardJavaScriptVersion;
import consulo.javascript.language.StandardJavaScriptVersions;
import consulo.language.version.LanguageVersion;
import consulo.util.lang.StringUtil;
import org.jspecify.annotations.Nullable;
import jakarta.inject.Singleton;

import java.util.ArrayList;
import java.util.List;

/**
 * @author VISTALL
 * @since 2017-12-23
 */
@Singleton
@ServiceImpl
public class StandardJavaScriptVersionsImpl extends StandardJavaScriptVersions {
    @Override
    public JavaScriptLanguageVersion getDefaultVersion() {
        LanguageVersion[] versions = JavaScriptLanguage.INSTANCE.getVersions();
        for (LanguageVersion version : versions) {
            if (version instanceof DefaultJavaScriptVersion) {
                return (JavaScriptLanguageVersion)version;
            }
        }
        return JavaScript15LanguageVersion.getInstance();
    }

    @Override
    public List<JavaScriptLanguageVersion> getValidLanguageVersions() {
        List<JavaScriptLanguageVersion> list = new ArrayList<>();
        LanguageVersion[] versions = JavaScriptLanguage.INSTANCE.getVersions();
        for (LanguageVersion version : versions) {
            if (version instanceof StandardJavaScriptVersion) {
                list.add((BaseJavaScriptLanguageVersion)version);
            }
        }

        list.sort((o1, o2) -> StringUtil.naturalCompare(o1.getPresentableName(), o2.getPresentableName()));
        return list;
    }

    @Override
    public JavaScriptLanguageVersion findVersionById(@Nullable String id) {
        if (StringUtil.isEmpty(id)) {
            return getDefaultVersion();
        }

        LanguageVersion[] versions = JavaScriptLanguage.INSTANCE.getVersions();
        for (LanguageVersion version : versions) {
            if (version instanceof StandardJavaScriptVersion && id.equals(version.getId())) {
                return (BaseJavaScriptLanguageVersion)version;
            }
        }
        return getDefaultVersion();
    }
}

package consulo.javascript.jsx.language;

import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.icon.JavaScriptIconGroup;
import consulo.javascript.language.JavaScriptFileTypeWithVersion;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.file.LanguageFileType;
import consulo.language.version.LanguageVersion;
import consulo.localize.LocalizeValue;
import consulo.module.Module;
import consulo.ui.image.Image;
import consulo.virtualFileSystem.VirtualFile;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * @author VISTALL
 * @since 2019-12-16
 */
public class JSXFileType extends LanguageFileType implements JavaScriptFileTypeWithVersion
{
	public static final JSXFileType INSTANCE = new JSXFileType();

	private JSXFileType()
	{
		super(JavaScriptLanguage.INSTANCE);
	}

	@Nonnull
	@Override
	public String getId()
	{
		return "JSX";
	}

	@Nonnull
	@Override
	public LocalizeValue getDescription()
	{
		return LocalizeValue.localizeTODO("JSX files");
	}

	@Nonnull
	@Override
	public String getDefaultExtension()
	{
		return "jsx";
	}

	@Nullable
	@Override
	public Image getIcon()
	{
		return JavaScriptIconGroup.jsx();
	}

	@RequiredReadAction
	@Nonnull
	@Override
	public LanguageVersion getLanguageVersion(@Nullable Module module, @Nullable VirtualFile virtualFile)
	{
		return JSXJavaScriptVersion.getInstance();
	}
}

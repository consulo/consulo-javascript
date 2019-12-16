package consulo.javascript.jsx.language;

import com.intellij.lang.javascript.JavaScriptIcons;
import com.intellij.openapi.fileTypes.LanguageFileType;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.vfs.VirtualFile;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.lang.JavaScriptFileTypeWithVersion;
import consulo.javascript.lang.JavaScriptLanguage;
import consulo.lang.LanguageVersion;
import consulo.ui.image.Image;

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
	public String getDescription()
	{
		return "JSX files";
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
		return JavaScriptIcons.Jsx;
	}

	@RequiredReadAction
	@Nonnull
	@Override
	public LanguageVersion getLanguageVersion(@Nullable Module module, @Nullable VirtualFile virtualFile)
	{
		return JSXJavaScriptVersion.getInstance();
	}
}

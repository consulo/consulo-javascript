package consulo.javascript.impl.copyright;

import consulo.javascript.language.JavaScriptFileType;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.copyright.UpdateCopyrightsProvider;
import consulo.language.copyright.UpdatePsiFileCopyright;
import consulo.language.copyright.config.CopyrightFileConfig;
import consulo.language.copyright.config.CopyrightProfile;
import consulo.language.copyright.ui.TemplateCommentPanel;
import consulo.language.psi.PsiFile;
import consulo.project.Project;
import consulo.virtualFileSystem.fileType.FileType;

import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 16.02.2015
 */
@ExtensionImpl
public class UpdateJavaScriptFileCopyrightProvider extends UpdateCopyrightsProvider<CopyrightFileConfig>
{
	@Nonnull
	@Override
	public FileType getFileType()
	{
		return JavaScriptFileType.INSTANCE;
	}

	@Nonnull
	@Override
	public UpdatePsiFileCopyright<CopyrightFileConfig> createInstance(@Nonnull PsiFile file, @Nonnull CopyrightProfile copyrightProfile)
	{
		return new UpdateJavaScriptFileCopyright(file, copyrightProfile);
	}

	@Nonnull
	@Override
	public CopyrightFileConfig createDefaultOptions()
	{
		return new CopyrightFileConfig();
	}

	@Nonnull
	@Override
	public TemplateCommentPanel createConfigurable(@Nonnull Project project, @Nonnull TemplateCommentPanel parentPane, @Nonnull FileType fileType)
	{
		return new TemplateCommentPanel(fileType, parentPane, project);
	}
}

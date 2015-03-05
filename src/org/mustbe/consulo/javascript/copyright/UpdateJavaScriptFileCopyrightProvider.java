package org.mustbe.consulo.javascript.copyright;

import org.jetbrains.annotations.NotNull;
import org.mustbe.consulo.copyright.config.CopyrightFileConfig;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFile;
import com.maddyhome.idea.copyright.CopyrightProfile;
import com.maddyhome.idea.copyright.psi.UpdateCopyrightsProvider;
import com.maddyhome.idea.copyright.psi.UpdatePsiFileCopyright;
import com.maddyhome.idea.copyright.ui.TemplateCommentPanel;

/**
 * @author VISTALL
 * @since 16.02.2015
 */
public class UpdateJavaScriptFileCopyrightProvider extends UpdateCopyrightsProvider<CopyrightFileConfig>
{
	@NotNull
	@Override
	public UpdatePsiFileCopyright<CopyrightFileConfig> createInstance(@NotNull PsiFile file, @NotNull CopyrightProfile copyrightProfile)
	{
		return new UpdateJavaScriptFileCopyright(file, copyrightProfile);
	}

	@NotNull
	@Override
	public CopyrightFileConfig createDefaultOptions()
	{
		return new CopyrightFileConfig();
	}

	@NotNull
	@Override
	public TemplateCommentPanel createConfigurable(@NotNull Project project, @NotNull TemplateCommentPanel parentPane, @NotNull FileType fileType)
	{
		return new TemplateCommentPanel(fileType, parentPane, project);
	}
}

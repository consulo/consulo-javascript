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


/**
 * @author VISTALL
 * @since 16.02.2015
 */
@ExtensionImpl
public class UpdateJavaScriptFileCopyrightProvider extends UpdateCopyrightsProvider<CopyrightFileConfig> {
    @Override
    public FileType getFileType() {
        return JavaScriptFileType.INSTANCE;
    }

    @Override
    public UpdatePsiFileCopyright<CopyrightFileConfig> createInstance(PsiFile file, CopyrightProfile copyrightProfile) {
        return new UpdateJavaScriptFileCopyright(file, copyrightProfile);
    }

    @Override
    public CopyrightFileConfig createDefaultOptions() {
        return new CopyrightFileConfig();
    }

    @Override
    public TemplateCommentPanel createConfigurable(
        Project project,
        TemplateCommentPanel parentPane,
        FileType fileType
    ) {
        return new TemplateCommentPanel(fileType, parentPane, project);
    }
}

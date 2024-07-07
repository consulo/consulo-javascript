package consulo.javascript.impl.ide.actions;

import consulo.javascript.language.JavaScriptFileType;
import consulo.annotation.access.RequiredReadAction;
import consulo.dataContext.DataContext;
import consulo.ide.IdeView;
import consulo.ide.action.CreateFileFromTemplateAction;
import consulo.ide.action.CreateFileFromTemplateDialog;
import consulo.javascript.module.extension.JavaScriptModuleExtension;
import consulo.language.editor.CommonDataKeys;
import consulo.language.psi.PsiDirectory;
import consulo.language.util.ModuleUtilCore;
import consulo.localize.LocalizeValue;
import consulo.module.Module;
import consulo.module.content.NewFileModuleResolver;
import consulo.project.Project;
import consulo.ui.annotation.RequiredUIAccess;
import consulo.virtualFileSystem.fileType.FileType;
import jakarta.annotation.Nullable;

/**
 * @author VISTALL
 * @since 06.12.2015
 */
public class JavaScriptCreateFileAction extends CreateFileFromTemplateAction
{
	public JavaScriptCreateFileAction()
	{
		super(LocalizeValue.empty(), LocalizeValue.empty(), JavaScriptFileType.INSTANCE.getIcon());
	}

	@Override
	@RequiredUIAccess
	protected boolean isAvailable(DataContext dataContext)
	{
		if(!super.isAvailable(dataContext))
		{
			return false;
		}
		Module module = findModule(dataContext);
		return module != null && ModuleUtilCore.getExtension(module, JavaScriptModuleExtension.class) != null;
	}

	@Nullable
	@Override
	protected FileType getFileTypeForModuleResolve()
	{
		return JavaScriptFileType.INSTANCE;
	}

	@RequiredReadAction
	private static Module findModule(DataContext dataContext)
	{
		Project project = dataContext.getData(CommonDataKeys.PROJECT);
		assert project != null;
		final IdeView view = dataContext.getData(IdeView.KEY);
		if(view == null)
		{
			return null;
		}

		final PsiDirectory directory = view.getOrChooseDirectory();
		if(directory == null)
		{
			return null;
		}

		Module resolvedModule = NewFileModuleResolver.resolveModule(directory.getProject(), directory.getVirtualFile(), JavaScriptFileType.INSTANCE);
		if(resolvedModule != null)
		{
			return resolvedModule;
		}
		return dataContext.getData(Module.KEY);
	}

	@Override
	protected void buildDialog(Project project, PsiDirectory directory, CreateFileFromTemplateDialog.Builder builder)
	{
		builder.setTitle("Create JavaScript File");

		builder.addKind("Empty File", JavaScriptFileType.INSTANCE.getIcon(), "JavaScriptFile");
	}

	@Override
	protected String getActionName(PsiDirectory directory, String newName, String templateName)
	{
		return "Create JavaScript File";
	}
}

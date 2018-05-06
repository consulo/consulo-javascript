package consulo.javascript.ide.actions;

import javax.annotation.Nullable;
import com.intellij.ide.IdeView;
import com.intellij.ide.actions.CreateFileFromTemplateAction;
import com.intellij.ide.actions.CreateFileFromTemplateDialog;
import com.intellij.lang.javascript.JavaScriptFileType;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.actionSystem.LangDataKeys;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiDirectory;
import consulo.annotations.RequiredDispatchThread;
import consulo.annotations.RequiredReadAction;
import consulo.awt.TargetAWT;
import consulo.javascript.module.extension.JavaScriptModuleExtension;

/**
 * @author VISTALL
 * @since 06.12.2015
 */
public class JavaScriptCreateFileAction extends CreateFileFromTemplateAction
{
	public JavaScriptCreateFileAction()
	{
		super(null, null, TargetAWT.to(JavaScriptFileType.INSTANCE.getIcon()));
	}

	@Override
	@RequiredDispatchThread
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
		final IdeView view = dataContext.getData(LangDataKeys.IDE_VIEW);
		if(view == null)
		{
			return null;
		}

		final PsiDirectory directory = view.getOrChooseDirectory();
		if(directory == null)
		{
			return null;
		}

		Module resolvedModule = ModuleResolver.EP_NAME.composite().resolveModule(directory, JavaScriptFileType.INSTANCE);
		if(resolvedModule != null)
		{
			return resolvedModule;
		}
		return dataContext.getData(LangDataKeys.MODULE);
	}

	@Override
	protected void buildDialog(Project project, PsiDirectory directory, CreateFileFromTemplateDialog.Builder builder)
	{
		builder.setTitle("Create JavaScript File");

		builder.addKind("Empty File", TargetAWT.to(JavaScriptFileType.INSTANCE.getIcon()), "JavaScriptFile");
	}

	@Override
	protected String getActionName(PsiDirectory directory, String newName, String templateName)
	{
		return "Create JavaScript File";
	}
}

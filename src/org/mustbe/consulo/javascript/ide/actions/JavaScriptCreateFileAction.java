package org.mustbe.consulo.javascript.ide.actions;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.RequiredDispatchThread;
import org.mustbe.consulo.RequiredReadAction;
import org.mustbe.consulo.javascript.module.extension.JavaScriptModuleExtension;
import org.mustbe.consulo.roots.ContentEntryFileListener;
import com.intellij.ide.IdeView;
import com.intellij.ide.actions.CreateFileFromTemplateAction;
import com.intellij.ide.actions.CreateFileFromTemplateDialog;
import com.intellij.lang.javascript.JavaScriptFileType;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.actionSystem.LangDataKeys;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileSystem;
import com.intellij.psi.PsiDirectory;
import com.intellij.testFramework.LightVirtualFile;

/**
 * @author VISTALL
 * @since 06.12.2015
 */
public class JavaScriptCreateFileAction extends CreateFileFromTemplateAction
{
	public JavaScriptCreateFileAction()
	{
		super(null, null, JavaScriptFileType.INSTANCE.getIcon());
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

	@RequiredReadAction
	private static Module findModule(DataContext dataContext)
	{
		Project project = CommonDataKeys.PROJECT.getData(dataContext);
		assert project != null;
		final IdeView view = LangDataKeys.IDE_VIEW.getData(dataContext);
		if(view == null)
		{
			return null;
		}

		final PsiDirectory orChooseDirectory = view.getOrChooseDirectory();
		if(orChooseDirectory == null)
		{
			return null;
		}

		Module resolve = findModuleByPsiDirectory(project, orChooseDirectory);
		if(resolve != null)
		{
			return resolve;
		}
		return LangDataKeys.MODULE.getData(dataContext);
	}

	@Nullable
	@RequiredReadAction
	private static Module findModuleByPsiDirectory(Project project, final PsiDirectory orChooseDirectory)
	{
		LightVirtualFile l = new LightVirtualFile("test.js", JavaScriptFileType.INSTANCE, "")
		{
			@Override
			public VirtualFile getParent()
			{
				return orChooseDirectory.getVirtualFile();
			}

			@NotNull
			@Override
			public VirtualFileSystem getFileSystem()
			{
				return LocalFileSystem.getInstance();
			}
		};
		for(ContentEntryFileListener.PossibleModuleForFileResolver o : ContentEntryFileListener.PossibleModuleForFileResolver.EP_NAME.getExtensions())
		{
			Module resolve = o.resolve(project, l);
			if(resolve != null)
			{
				return resolve;
			}
		}
		return null;
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

package com.intellij.lang.javascript.inspections;

import java.util.Properties;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.ide.fileTemplates.FileTemplate;
import com.intellij.ide.fileTemplates.FileTemplateManager;
import com.intellij.ide.fileTemplates.FileTemplateUtil;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.flex.ImportUtils;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDirectory;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.util.text.StringTokenizer;

/**
 * Created by IntelliJ IDEA.
 * User: Maxim.Mossienko
 * Date: 11.02.2009
 * Time: 14:01:34
 * To change this template use File | Settings | File Templates.
 */
class CreateClassOrInterfaceAction implements LocalQuickFix
{
	private final String classNameToCreate;
	private final JSReferenceExpression myContext;
	private String packageName;
	private final boolean myIsInterface;

	public CreateClassOrInterfaceAction(JSReferenceExpression context, boolean isInterface)
	{
		classNameToCreate = context.getReferencedName();
		myContext = context;
		myIsInterface = isInterface;
	}

	@Override
	@NotNull
	public String getName()
	{
		final String key = myIsInterface ? "javascript.create.interface.intention.name" : "javascript.create.class.intention.name";
		return JSBundle.message(key, classNameToCreate);
	}

	@Override
	@NotNull
	public String getFamilyName()
	{
		return getName();
	}

	@Override
	public void applyFix(@NotNull final Project project, @NotNull final ProblemDescriptor descriptor)
	{
		PsiFile contextFile = myContext.getContainingFile();
		final PsiElement context = contextFile.getContext();
		if(context != null)
		{
			contextFile = context.getContainingFile();
		}

		packageName = JSResolveUtil.getExpectedPackageNameFromFile(contextFile.getVirtualFile(), project, false);

		if(!ApplicationManager.getApplication().isUnitTestMode())
		{
			final CreateClassDialog dialog = new CreateClassDialog(project, classNameToCreate, packageName, myIsInterface);
			dialog.show();
			if(dialog.getExitCode() != DialogWrapper.OK_EXIT_CODE)
			{
				return;
			}
			packageName = dialog.getPackageName().trim();
		}
		else
		{
			packageName = "foo";
		}

		final PsiFile contextFile1 = contextFile;
		ApplicationManager.getApplication().runWriteAction(new Runnable()
		{
			@Override
			public void run()
			{
				try
				{
					final FileTemplate template = FileTemplateManager.getInstance().getTemplate(myIsInterface ? JavaScriptSupportLoader
							.ACTION_SCRIPT_INTERFACE_TEMPLATE_NAME : JavaScriptSupportLoader.ACTION_SCRIPT_CLASS_TEMPLATE_NAME);
					@NonNls final String fileName = classNameToCreate + ".as";
					final Properties props = new Properties();
					props.setProperty(FileTemplate.ATTRIBUTE_NAME, classNameToCreate);
					final Module element = ModuleUtil.findModuleForPsiElement(contextFile1);
					VirtualFile base = ModuleRootManager.getInstance(element).getSourceRoots()[0];
					VirtualFile relativeFile = VfsUtil.findRelativeFile(packageName, base);

					if(relativeFile == null)
					{
						relativeFile = base;
						StringTokenizer tokenizer = new StringTokenizer(packageName, ".");
						while(tokenizer.hasMoreTokens())
						{
							String nextNameSegment = tokenizer.nextToken();
							VirtualFile next = relativeFile.findChild(nextNameSegment);
							if(next == null)
							{
								next = relativeFile.createChildDirectory(this, nextNameSegment);
							}
							relativeFile = next;
						}
					}

					assert relativeFile != null;
					props.setProperty(FileTemplate.ATTRIBUTE_PACKAGE_NAME, packageName);
					final PsiDirectory psiDirectory = PsiManager.getInstance(project).findDirectory(relativeFile);
					assert psiDirectory != null;
					FileTemplateUtil.createFromTemplate(template, fileName, props, psiDirectory);

					String contextPackage = JSResolveUtil.findPackageStatementQualifier(myContext);
					if(packageName != null && !packageName.equals(contextPackage) && packageName.length() > 0)
					{
						ImportUtils.doImport(myContext, packageName + "." + classNameToCreate);
					}
				}
				catch(Exception e)
				{
					Logger.getInstance(getClass().getName()).error(e);
				}
			}
		});
	}
}

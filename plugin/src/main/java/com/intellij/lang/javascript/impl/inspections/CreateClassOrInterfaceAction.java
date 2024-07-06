/*
 * Copyright 2000-2005 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.intellij.lang.javascript.impl.inspections;

import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.impl.flex.ImportUtils;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.application.ApplicationManager;
import consulo.fileTemplate.FileTemplate;
import consulo.fileTemplate.FileTemplateManager;
import consulo.fileTemplate.FileTemplateUtil;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.editor.inspection.LocalQuickFix;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiDirectory;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiManager;
import consulo.language.util.ModuleUtilCore;
import consulo.logging.Logger;
import consulo.module.Module;
import consulo.module.content.ModuleRootManager;
import consulo.project.Project;
import consulo.ui.ex.awt.DialogWrapper;
import consulo.util.lang.text.StringTokenizer;
import consulo.virtualFileSystem.VirtualFile;
import consulo.virtualFileSystem.util.VirtualFileUtil;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;
import java.util.Properties;

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
	@Nonnull
	public String getName()
	{
		return myIsInterface 
			? JavaScriptLocalize.javascriptCreateInterfaceIntentionName(classNameToCreate).get() 
			: JavaScriptLocalize.javascriptCreateClassIntentionName(classNameToCreate).get();
	}

	@Override
	@Nonnull
	public String getFamilyName()
	{
		return getName();
	}

	@Override
	public void applyFix(@Nonnull final Project project, @Nonnull final ProblemDescriptor descriptor)
	{
		PsiFile contextFile = myContext.getContainingFile();
		final PsiElement context = contextFile.getContext();
		if (context != null)
		{
			contextFile = context.getContainingFile();
		}

		packageName = JSResolveUtil.getExpectedPackageNameFromFile(contextFile.getVirtualFile(), project, false);

		if (!ApplicationManager.getApplication().isUnitTestMode())
		{
			final CreateClassDialog dialog = new CreateClassDialog(project, classNameToCreate, packageName, myIsInterface);
			dialog.show();
			if (dialog.getExitCode() != DialogWrapper.OK_EXIT_CODE)
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
					final FileTemplate template = FileTemplateManager.getInstance().getTemplate(
						myIsInterface
							? JavaScriptSupportLoader.ACTION_SCRIPT_INTERFACE_TEMPLATE_NAME
							: JavaScriptSupportLoader.ACTION_SCRIPT_CLASS_TEMPLATE_NAME
					);
					@NonNls final String fileName = classNameToCreate + ".as";
					final Properties props = new Properties();
					props.setProperty(FileTemplate.ATTRIBUTE_NAME, classNameToCreate);
					final Module element = ModuleUtilCore.findModuleForPsiElement(contextFile1);
					VirtualFile base = ModuleRootManager.getInstance(element).getSourceRoots()[0];
					VirtualFile relativeFile = VirtualFileUtil.findRelativeFile(packageName, base);

					if (relativeFile == null)
					{
						relativeFile = base;
						StringTokenizer tokenizer = new StringTokenizer(packageName, ".");
						while (tokenizer.hasMoreTokens())
						{
							String nextNameSegment = tokenizer.nextToken();
							VirtualFile next = relativeFile.findChild(nextNameSegment);
							if (next == null)
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
					if (packageName != null && !packageName.equals(contextPackage) && packageName.length() > 0)
					{
						ImportUtils.doImport(myContext, packageName + "." + classNameToCreate);
					}
				}
				catch (Exception e)
				{
					Logger.getInstance(getClass().getName()).error(e);
				}
			}
		});
	}
}

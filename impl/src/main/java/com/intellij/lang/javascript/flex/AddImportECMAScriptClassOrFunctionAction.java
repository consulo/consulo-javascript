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

package com.intellij.lang.javascript.flex;

import gnu.trove.THashSet;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.codeInsight.hint.HintManager;
import com.intellij.codeInsight.hint.QuestionAction;
import com.intellij.codeInsight.navigation.NavigationUtil;
import com.intellij.codeInspection.HintAction;
import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.ide.util.PsiElementListCellRenderer;
import com.intellij.lang.injection.InjectedLanguageManager;
import com.intellij.lang.javascript.JavaScriptBundle;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSQualifiedNamedElement;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.impl.JSPsiImplUtils;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiPolyVariantReference;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.PsiElementProcessor;
import com.intellij.util.SmartList;

/**
 * @author Maxim.Mossienko
 *         Date: Apr 25, 2008
 *         Time: 8:36:38 PM
 */
public class AddImportECMAScriptClassOrFunctionAction implements HintAction, QuestionAction, LocalQuickFix
{
	private final PsiPolyVariantReference myReference;
	private Editor myEditor;
	private boolean isAvailable;
	private boolean isAvailableCalculated;
	private long modificationCount = -1;
	private String calculatedClass;
	@NonNls
	private static final String HAS_MORE_SUFFIX = ", ...";
	private boolean isFunction;

	public AddImportECMAScriptClassOrFunctionAction(Editor editor, final PsiPolyVariantReference psiReference)
	{
		myReference = psiReference;
		myEditor = editor;
	}

	@Override
	public boolean showHint(final Editor editor)
	{
		myEditor = editor;
		final PsiElement element = myReference.getElement();
		TextRange textRange = InjectedLanguageManager.getInstance(element.getProject()).injectedToHost(element, element.getTextRange());
		HintManager.getInstance().showQuestionHint(editor, getText(), textRange.getStartOffset(), textRange.getEndOffset(), this);
		return true;
	}

	@Override
	@NotNull
	public String getText()
	{
		return JavaScriptBundle.message(isFunction ? "flex.import.function" : "flex.import.class", calculatedClass);
	}

	@Override
	@NotNull
	public String getName()
	{
		return getText();
	}

	@Override
	@NotNull
	public String getFamilyName()
	{
		return getText();
	}

	@Override
	public void applyFix(@NotNull final Project project, @NotNull final ProblemDescriptor descriptor)
	{
		invoke(project, myEditor, descriptor.getPsiElement().getContainingFile());
	}

	@Override
	public boolean isAvailable(@NotNull final Project project, final Editor editor, final PsiFile file)
	{
		if(!myReference.getElement().isValid())
		{
			return false;
		}
		final long modL = myReference.getElement().getManager().getModificationTracker().getModificationCount();

		if(!isAvailableCalculated || modL != modificationCount)
		{
			final ResolveResult[] results = myReference.multiResolve(false);
			boolean hasValidResult = false;

			for(ResolveResult r : results)
			{
				if(r.isValidResult())
				{
					hasValidResult = true;
					break;
				}
			}

			if(!hasValidResult)
			{
				final Collection<JSQualifiedNamedElement> candidates = getCandidates(editor, file);

				isAvailableCalculated = true;
				isAvailable = candidates.size() > 0;
				if(isAvailable)
				{
					final JSQualifiedNamedElement element = candidates.iterator().next();
					calculatedClass = element.getQualifiedName();
					isFunction = element instanceof JSFunction;
					if(candidates.size() > 1)
					{
						calculatedClass += HAS_MORE_SUFFIX;
					}
				}
				else
				{
					calculatedClass = "";
				}
			}
			else
			{
				isAvailableCalculated = true;
				isAvailable = false;
			}

			modificationCount = modL;
		}

		return isAvailable;
	}

	private Collection<JSQualifiedNamedElement> getCandidates(Editor editor, PsiFile file)
	{
		final Collection<JSQualifiedNamedElement> candidates;

		if(myReference instanceof JSReferenceExpression && ((JSReferenceExpression) myReference).getQualifier() == null)
		{
			Collection<JSQualifiedNamedElement> c = getCandidates(editor, file, myReference.getCanonicalText());
			filterDefaultPackage(c);
			candidates = new THashSet<JSQualifiedNamedElement>(c, JSPsiImplUtils.QUALIFIED_NAME_HASHING_STRATEGY);
		}
		else
		{
			JSQualifiedNamedElement invalidResult = null;

			for(ResolveResult r : myReference.multiResolve(false))
			{
				PsiElement element = r.getElement();
				if(element instanceof JSQualifiedNamedElement)
				{
					invalidResult = (JSQualifiedNamedElement) element;
				}
			}
			if(invalidResult != null)
			{
				candidates = new SmartList<JSQualifiedNamedElement>();
				candidates.add(invalidResult);
			}
			else
			{
				candidates = Collections.emptyList();
			}
		}
		return candidates;
	}

	public static Collection<JSQualifiedNamedElement> getCandidates(final Editor editor, final PsiFile file, final String name)
	{
		final Module element = ModuleUtil.findModuleForPsiElement(file);
		if(element != null)
		{
			return JSResolveUtil.findElementsByName(name, editor.getProject(), GlobalSearchScope.moduleWithDependenciesAndLibrariesScope(element, false));
		}
		else
		{
			return Collections.emptyList();
		}
	}

	@Override
	public void invoke(@NotNull final Project project, final Editor editor, final PsiFile file)
	{
		final Collection<JSQualifiedNamedElement> candidates = getCandidates(editor, file);

		if(candidates.size() > 0)
		{
			if(candidates.size() > 1)
			{
				NavigationUtil.getPsiElementPopup(candidates.toArray(new JSQualifiedNamedElement[candidates.size()]),
						new PsiElementListCellRenderer<JSQualifiedNamedElement>()
						{
							@Override
							public String getElementText(final JSQualifiedNamedElement element)
							{
								return element.getName();
							}

							@Override
							protected String getContainerText(final JSQualifiedNamedElement element, final String name)
							{
								final String qName = element.getQualifiedName();
								final String elementName = element.getName();
								String s = qName.equals(elementName) ? "" : qName.substring(0, qName.length() - elementName.length() - 1);
								if("".equals(s))
								{
									s = element.getContainingFile().getName();
								}
								return "( " + s + " )";
							}

							@Override
							protected int getIconFlags()
							{
								return 0;
							}
						}, JavaScriptBundle.message("choose.class.title"), new PsiElementProcessor<JSQualifiedNamedElement>()
						{
							@Override
							public boolean execute(final JSQualifiedNamedElement element)
							{
								CommandProcessor.getInstance().executeCommand(project, new Runnable()
								{
									@Override
									public void run()
									{
										doImport(editor, element.getQualifiedName());
									}
								}, getClass().getName(), this);

								return false;
							}
						}
				).showInBestPositionFor(editor);
			}
			else
			{
				doImport(editor, candidates.iterator().next().getQualifiedName());
			}
		}
	}

	private void doImport(final Editor editor, final String qName)
	{
		ApplicationManager.getApplication().runWriteAction(new Runnable()
		{
			@Override
			public void run()
			{
				final PsiElement element = myReference.getElement();
				ImportUtils.doImport(element, qName);
			}
		});
	}

	@Override
	public boolean startInWriteAction()
	{
		return false;
	}

	@Override
	public boolean execute()
	{
		final PsiFile containingFile = myReference.getElement().getContainingFile();
		invoke(containingFile.getProject(), myEditor, containingFile);

		return true;
	}

	private static void filterDefaultPackage(Collection<JSQualifiedNamedElement> candidates)
	{
		for(Iterator<JSQualifiedNamedElement> i = candidates.iterator(); i.hasNext(); )
		{
			if(!i.next().getQualifiedName().contains("."))
			{
				i.remove();
			}
		}
	}


}

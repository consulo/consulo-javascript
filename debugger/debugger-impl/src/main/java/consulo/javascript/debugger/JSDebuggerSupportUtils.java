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

package consulo.javascript.debugger;

import consulo.javascript.impl.language.psi.JSElementFactory;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.JSNamedElement;
import consulo.document.Document;
import consulo.document.FileDocumentManager;
import consulo.document.util.TextRange;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.inject.InjectedLanguageManager;
import consulo.language.psi.*;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.project.Project;
import consulo.util.lang.ref.Ref;
import consulo.virtualFileSystem.VirtualFile;
import consulo.xml.lang.xml.XMLLanguage;
import jakarta.annotation.Nonnull;

import jakarta.annotation.Nullable;
import java.util.List;

/**
 * User: Maxim.Mossienko
 * Date: Feb 27, 2008
 * Time: 7:45:08 PM
 */
public class JSDebuggerSupportUtils
{
	@Nullable
	public static TextRange getExpressionAtOffset(@Nonnull Project project, @Nonnull Document document, final int offset)
	{
		PsiFile file = PsiDocumentManager.getInstance(project).getPsiFile(document);
		if(file == null)
		{
			return null;
		}

		int injectionOffsetCorrection = 0;
		PsiReference ref = file.findReferenceAt(offset);

		if(ref == null)
		{
			final PsiElement at = file.findElementAt(offset);

			TextRange rangeForNamedElement = getRangeForNamedElement(at, 0);
			if(rangeForNamedElement != null)
			{
				return rangeForNamedElement;
			}

			final PsiLanguageInjectionHost psiLanguageInjectionHost = PsiTreeUtil.getParentOfType(at, PsiLanguageInjectionHost.class);

			if(psiLanguageInjectionHost != null)
			{
				final Ref<PsiReference> result = new Ref<PsiReference>();
				final Ref<PsiElement> eltInInjected = new Ref<PsiElement>();
				final int[] injectedOffset = new int[1];

				InjectedLanguageManager.getInstance(project).enumerate(psiLanguageInjectionHost, new PsiLanguageInjectionHost.InjectedPsiVisitor()
				{
					@Override
					public void visit(@Nonnull final PsiFile injectedPsi, @Nonnull final List<PsiLanguageInjectionHost.Shred> places)
					{
						final PsiLanguageInjectionHost.Shred shred = places.get(0);
						final int injectedStart = shred.getRangeInsideHost().getStartOffset() + shred.getHost().getTextOffset();
						final int offsetInInjected = offset - injectedStart;

						result.set(injectedPsi.findReferenceAt(offsetInInjected));
						eltInInjected.set(injectedPsi.findElementAt(offsetInInjected));
						injectedOffset[0] = injectedStart;
					}
				});

				ref = result.get();

				if(ref == null)
				{
					rangeForNamedElement = getRangeForNamedElement(eltInInjected.get(), injectedOffset[0]);
					if(rangeForNamedElement != null)
					{
						return rangeForNamedElement;
					}
				}
				else
				{
					injectionOffsetCorrection = injectedOffset[0];
				}
			}

			if(ref == null)
			{
				return null;
			}
		}

		final PsiElement element = ref.getElement();
		if(!element.getLanguage().isKindOf(JavaScriptLanguage.INSTANCE))
		{
			return null;
		}

		return element.getTextRange().shiftRight(injectionOffsetCorrection);
	}

	private static TextRange getRangeForNamedElement(final PsiElement at, int offset)
	{
		final PsiElement parent = at != null ? at.getParent() : null;

		if(parent instanceof JSNamedElement)
		{
			final PsiElement node = ((JSNamedElement) parent).getNameIdentifier();

			if(node != null)
			{
				return node.getTextRange().shiftRight(offset);
			}
		}
		return null;
	}

	public static Document createDocument(final String text, final Project project, @Nullable VirtualFile contextVirtualFile, int contextOffset)
	{
		PsiElement context = null;
		if(contextVirtualFile != null)
		{
			context = getContextElement(contextVirtualFile, contextOffset, project);
		}
		JSFile file = JSElementFactory.createExpressionCodeFragment(project, text, context, true);
		return PsiDocumentManager.getInstance(project).getDocument(file);
	}

	@Nullable
	public static PsiElement getContextElement(VirtualFile virtualFile, int offset, final @Nonnull Project project)
	{
		Document document = FileDocumentManager.getInstance().getDocument(virtualFile);
		PsiFile file = PsiManager.getInstance(project).findFile(virtualFile);
		if(file == null || document == null)
		{
			return null;
		}

		if(offset < 0)
		{
			offset = 0;
		}
		if(offset > document.getTextLength())
		{
			offset = document.getTextLength();
		}
		int startOffset = offset;

		int lineEndOffset = document.getLineEndOffset(document.getLineNumber(offset));
		PsiElement result = null;
		do
		{
			PsiElement element = file.findElementAt(offset);
			if(!(element instanceof PsiWhiteSpace) && !(element instanceof PsiComment))
			{
				result = element;
				break;
			}

			offset = element.getTextRange().getEndOffset() + 1;
		}
		while(offset < lineEndOffset);

		if(result == null)
		{
			result = file.findElementAt(startOffset);
		}

		if(result != null && result.getLanguage() == XMLLanguage.INSTANCE)
		{
			PsiLanguageInjectionHost parent = PsiTreeUtil.getParentOfType(result, PsiLanguageInjectionHost.class);

			if(parent != null)
			{
				final int finalOffset = offset;
				final Ref<PsiElement> resultInInjected = new Ref<PsiElement>();

				InjectedLanguageManager.getInstance(project).enumerate(parent, new PsiLanguageInjectionHost.InjectedPsiVisitor()
				{
					@Override
					public void visit(@Nonnull final PsiFile injectedPsi, @Nonnull final List<PsiLanguageInjectionHost.Shred> places)
					{
						final PsiLanguageInjectionHost.Shred shred = places.get(0);
						final int injectedStart = shred.getRangeInsideHost().getStartOffset() + shred.getHost().getTextOffset();
						final int offsetInInjected = finalOffset - injectedStart;

						resultInInjected.set(injectedPsi.findElementAt(offsetInInjected));
					}
				});

				result = resultInInjected.get();
			}
		}
		return result;
	}
}

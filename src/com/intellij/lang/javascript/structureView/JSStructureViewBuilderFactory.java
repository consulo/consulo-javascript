/*
 * @author max
 */
package com.intellij.lang.javascript.structureView;

import org.jetbrains.annotations.NotNull;
import com.intellij.ide.structureView.StructureViewBuilder;
import com.intellij.ide.structureView.StructureViewModel;
import com.intellij.ide.structureView.TreeBasedStructureViewBuilder;
import com.intellij.lang.PsiStructureViewFactory;
import com.intellij.psi.PsiFile;

public class JSStructureViewBuilderFactory implements PsiStructureViewFactory
{
	@Override
	public StructureViewBuilder getStructureViewBuilder(final PsiFile psiFile)
	{
		return new TreeBasedStructureViewBuilder()
		{
			@Override
			@NotNull
			public StructureViewModel createStructureViewModel()
			{
				return new JSStructureViewModel(psiFile);
			}

			@Override
			public boolean isRootNodeShown()
			{
				return false;
			}
		};
	}
}
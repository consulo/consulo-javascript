package com.intellij.lang.javascript.psi.impl;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.lang.javascript.psi.JSExpressionCodeFragment;
import com.intellij.openapi.fileTypes.FileTypeManager;
import com.intellij.openapi.project.Project;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiManager;
import com.intellij.psi.SingleRootFileViewProvider;
import com.intellij.psi.impl.PsiManagerEx;
import com.intellij.psi.impl.file.impl.FileManager;
import com.intellij.psi.impl.source.tree.FileElement;
import com.intellij.testFramework.LightVirtualFile;

/**
 * @author nik
 */
public class JSExpressionCodeFragmentImpl extends JSFileImpl implements JSExpressionCodeFragment
{
	private PsiElement myContext;
	private boolean myPhysical;
	private FileViewProvider myViewProvider;

	public JSExpressionCodeFragmentImpl(Project project, @NonNls String name, CharSequence text, boolean isPhysical)
	{
		super(((PsiManagerEx) PsiManager.getInstance(project)).getFileManager().createFileViewProvider(new LightVirtualFile(name,
				FileTypeManager.getInstance().getFileTypeByFileName(name), text), isPhysical));
		myPhysical = isPhysical;
		((SingleRootFileViewProvider) getViewProvider()).forceCachedPsi(this);
	}

	//todo[nik] extract these methods from PsiCodeFragmentImpl?
	@Override
	protected JSExpressionCodeFragmentImpl clone()
	{
		final JSExpressionCodeFragmentImpl clone = (JSExpressionCodeFragmentImpl) cloneImpl((FileElement) calcTreeElement().clone());
		clone.myPhysical = false;
		clone.myOriginalFile = this;
		FileManager fileManager = ((PsiManagerEx) getManager()).getFileManager();
		SingleRootFileViewProvider cloneViewProvider = (SingleRootFileViewProvider) fileManager.createFileViewProvider(new LightVirtualFile(getName(),
				getLanguage(), getText()), false);
		cloneViewProvider.forceCachedPsi(clone);
		clone.myViewProvider = cloneViewProvider;
		return clone;
	}

	@Override
	public PsiElement getContext()
	{
		return myContext;
	}

	@Override
	@NotNull
	public FileViewProvider getViewProvider()
	{
		if(myViewProvider != null)
		{
			return myViewProvider;
		}
		return super.getViewProvider();
	}

	@Override
	public boolean isValid()
	{
		if(!super.isValid())
		{
			return false;
		}
		if(myContext != null && !myContext.isValid())
		{
			return false;
		}
		return true;
	}

	@Override
	public boolean isPhysical()
	{
		return myPhysical;
	}

	public void setContext(PsiElement context)
	{
		myContext = context;
	}

}

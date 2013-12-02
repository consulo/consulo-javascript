package com.intellij.lang.javascript.index;

import org.jetbrains.annotations.NonNls;
import com.intellij.lang.javascript.psi.JSNamedElement;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiNamedElement;

/**
 * @by yole, maxim.mossienko
 */
public interface JavaScriptSymbolProcessor
{
	boolean processFunction(JSNamespace namespace, final int nameId, JSNamedElement function);

	boolean processClass(JSNamespace namespace, final int nameId, JSNamedElement clazz);

	boolean processVariable(JSNamespace namespace, final int nameId, JSNamedElement variable);

	boolean acceptsFile(PsiFile file);

	PsiFile getBaseFile();

	boolean processProperty(final JSNamespace namespace, final int nameId, final JSNamedElement property);

	boolean processDefinition(final JSNamespace namespace, final int nameId, final JSNamedElement refExpr);

	boolean processNamespace(final JSNamespace namespace, final int nameId, final JSNamedElement refExpr);

	boolean processImplicitNamespace(final JSNamespace namespace, final int nameId, final PsiElement refExpr, boolean finalReference);

	boolean processImplicitFunction(final JSNamespace namespace, final int nameId, final PsiElement refExpr);

	boolean processImplicitVariable(final JSNamespace namespace, final int nameId, final PsiElement refExpr);

	int getRequiredNameId();

	boolean processTag(JSNamespace namespace, final int nameId, PsiNamedElement namedElement, @NonNls final String attrName);

	abstract class DefaultSymbolProcessor implements JavaScriptSymbolProcessor
	{
		protected PsiFile currentFile;

		public boolean processFunction(final JSNamespace namespace, final int nameId, final JSNamedElement function)
		{
			return process(function, namespace);
		}

		public boolean processClass(final JSNamespace namespace, final int nameId, final JSNamedElement clazz)
		{
			return process(clazz, namespace);
		}

		public boolean processVariable(final JSNamespace namespace, final int nameId, final JSNamedElement variable)
		{
			return process(variable, namespace);
		}

		public boolean acceptsFile(final PsiFile file)
		{
			currentFile = file;
			return true;
		}

		public boolean processProperty(final JSNamespace namespace, final int nameId, final JSNamedElement property)
		{
			return process(property, namespace);
		}

		public boolean processDefinition(final JSNamespace namespace, final int nameId, final JSNamedElement refExpr)
		{
			return process(refExpr, namespace);
		}

		public boolean processNamespace(final JSNamespace namespace, final int nameId, final JSNamedElement refExpr)
		{
			return process(refExpr, namespace);
		}

		public boolean processImplicitNamespace(final JSNamespace namespace, final int nameId, final PsiElement refExpr, boolean finalReference)
		{
			return process(refExpr, namespace);
		}

		public boolean processImplicitFunction(final JSNamespace namespace, final int nameId, final PsiElement refExpr)
		{
			return process(refExpr, namespace);
		}

		public boolean processImplicitVariable(final JSNamespace namespace, final int nameId, final PsiElement refExpr)
		{
			return process(refExpr, namespace);
		}

		public boolean processTag(final JSNamespace namespace, final int nameId, final PsiNamedElement namedElement, @NonNls final String attrName)
		{
			return process(namedElement, namespace);
		}

		protected abstract boolean process(final PsiElement namedElement, final JSNamespace namespace);
	}
}

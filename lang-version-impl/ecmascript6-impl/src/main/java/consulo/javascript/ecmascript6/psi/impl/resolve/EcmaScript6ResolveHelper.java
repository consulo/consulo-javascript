package consulo.javascript.ecmascript6.psi.impl.resolve;

import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.impl.JSReferenceExpressionImpl;
import com.intellij.lang.javascript.psi.resolve.JSImportedElementResolveResult;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.BaseScopeProcessor;
import com.intellij.psi.util.PsiTreeUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.ecmascript6.psi.ES6ImportDeclaration;
import consulo.javascript.ecmascript6.psi.ES6ImportedBinding;
import consulo.javascript.lang.psi.impl.resolve.ResolveHelper;
import consulo.util.lang.Comparing;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * @author VISTALL
 * @since 2019-12-14
 */
public class EcmaScript6ResolveHelper extends ResolveHelper
{
	public static final ResolveHelper INSTANCE = new EcmaScript6ResolveHelper();

	private static class ImportProcessor extends BaseScopeProcessor
	{
		private final String myReferencedName;
		private JSImportedElementResolveResult myResult;

		public ImportProcessor(String referencedName)
		{
			myReferencedName = referencedName;
		}

		@Override
		public boolean execute(@Nonnull PsiElement psiElement, ResolveState resolveState)
		{
			if(psiElement instanceof ES6ImportedBinding && myReferencedName.equals(((ES6ImportedBinding) psiElement).getName()))
			{
				myResult = new JSImportedElementResolveResult(myReferencedName, psiElement, PsiTreeUtil.getParentOfType(psiElement, ES6ImportDeclaration.class));
				return false;
			}
			return true;
		}
	}

	@Nullable
	@Override
	public JSImportedElementResolveResult resolveTypeNameUsingImports(@Nonnull String referencedName, PsiNamedElement parent)
	{
		ImportProcessor processor = new ImportProcessor(referencedName);

		PsiFile containingFile = parent.getContainingFile();
		if(containingFile instanceof JSFile)
		{
			ES6ImportDeclaration[] childrenOfType = PsiTreeUtil.getChildrenOfType(containingFile, ES6ImportDeclaration.class);
			if(childrenOfType == null)
			{
				return null;
			}

			for(ES6ImportDeclaration declaration : childrenOfType)
			{
				if(!declaration.processDeclarations(processor, ResolveState.initial(), declaration, parent))
				{
					break;
				}
			}
		}

		return processor.myResult;
	}

	@Override
	public boolean execute(ResolveProcessor resolveProcessor, PsiElement element, ResolveState state)
	{
		if(element instanceof ES6ImportedBinding && Comparing.equal(((ES6ImportedBinding) element).getName(), resolveProcessor.getName()))
		{
			return true;
		}
		return false;
	}

	@RequiredReadAction
	@Override
	public boolean isResolveTo(JSReferenceExpressionImpl expression, PsiElement element)
	{
		if(element instanceof ES6ImportedBinding && Comparing.equal(expression.getReferencedName(), ((ES6ImportedBinding) element).getName()))
		{
			return true;
		}
		return super.isResolveTo(expression, element);
	}
}

package consulo.javascript.ecmascript.psi.impl.resolve;

import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.impl.JSReferenceExpressionImpl;
import com.intellij.lang.javascript.psi.resolve.JSImportedElementResolveResult;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.ecmascript.psi.ES6ImportDeclaration;
import consulo.javascript.ecmascript.psi.ES6ImportedBinding;
import consulo.javascript.lang.psi.impl.resolve.ResolveHelper;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.resolve.BaseScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.util.lang.Comparing;
import jakarta.annotation.Nonnull;

import jakarta.annotation.Nullable;

/**
 * @author VISTALL
 * @since 2019-12-14
 */
public class EcmaScript6ResolveHelper extends ResolveHelper {
    public static final ResolveHelper INSTANCE = new EcmaScript6ResolveHelper();

    private static class ImportProcessor extends BaseScopeProcessor {
        private final String myReferencedName;
        private JSImportedElementResolveResult myResult;

        public ImportProcessor(String referencedName) {
            myReferencedName = referencedName;
        }

        @Override
        public boolean execute(@Nonnull PsiElement psiElement, ResolveState resolveState) {
            if (psiElement instanceof ES6ImportedBinding es6ImportedBinding && myReferencedName.equals(es6ImportedBinding.getName())) {
                myResult = new JSImportedElementResolveResult(
                    myReferencedName,
                    psiElement,
                    PsiTreeUtil.getParentOfType(psiElement, ES6ImportDeclaration.class)
                );
                return false;
            }
            return true;
        }
    }

    @Nullable
    @Override
    public JSImportedElementResolveResult resolveTypeNameUsingImports(@Nonnull String referencedName, PsiNamedElement parent) {
        ImportProcessor processor = new ImportProcessor(referencedName);

        PsiFile containingFile = parent.getContainingFile();
        if (containingFile instanceof JSFile) {
            ES6ImportDeclaration[] childrenOfType = PsiTreeUtil.getChildrenOfType(containingFile, ES6ImportDeclaration.class);
            if (childrenOfType == null) {
                return null;
            }

            for (ES6ImportDeclaration declaration : childrenOfType) {
                if (!declaration.processDeclarations(processor, ResolveState.initial(), declaration, parent)) {
                    break;
                }
            }
        }

        return processor.myResult;
    }

    @Override
    public boolean execute(ResolveProcessor resolveProcessor, PsiElement element, ResolveState state) {
        if (element instanceof ES6ImportedBinding es6ImportedBinding
            && Comparing.equal(es6ImportedBinding.getName(), resolveProcessor.getName())) {
            return true;
        }
        return false;
    }

    @RequiredReadAction
    @Override
    public boolean isResolveTo(JSReferenceExpressionImpl expression, PsiElement element) {
        if (element instanceof ES6ImportedBinding es6ImportedBinding
            && Comparing.equal(expression.getReferencedName(), es6ImportedBinding.getName())) {
            return true;
        }
        return super.isResolveTo(expression, element);
    }
}

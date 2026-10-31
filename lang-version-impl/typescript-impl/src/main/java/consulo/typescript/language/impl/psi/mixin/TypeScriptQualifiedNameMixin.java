package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.ecmal4.JSReferenceListMember;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import consulo.annotation.access.RequiredReadAction;
import consulo.document.util.TextRange;
import consulo.javascript.language.psi.stub.JavaScriptIndexKeys;
import com.intellij.lang.javascript.psi.JSClass;
import consulo.language.ast.ASTNode;
import consulo.language.ast.TokenSet;
import consulo.language.psi.*;
import consulo.language.psi.resolve.ResolveCache;
import consulo.javascript.psi.JSTypeOnlyDeclaration;
import com.intellij.lang.javascript.psi.JSQualifiedNamedElement;
import consulo.javascript.impl.library.JavaScriptLibraries;
import consulo.language.psi.stub.StubIndex;
import consulo.language.util.IncorrectOperationException;
import consulo.typescript.language.impl.psi.TypeScriptElementImpl;
import consulo.typescript.language.impl.psi.TypeScriptModuleUtil;
import consulo.typescript.language.impl.psi.TypeScriptResolveUtil;
import consulo.typescript.language.psi.TypeScriptTypes;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

/**
 * {@code A.B.C} of a type reference ({@code x: ns.Foo}, {@code Array<Foo>}, {@code implements Foo}): a reference in the
 * type namespace - classes, interfaces, enums, type aliases, namespaces, type parameters, imports. The first name is
 * resolved up the scopes, the others as members of the namespace, enum or module before them.
 *
 * @since 2026-10-31
 */
public class TypeScriptQualifiedNameMixin extends TypeScriptElementImpl implements PsiPolyVariantReference, JSReferenceListMember {
    private static final TokenSet IDENTIFIERS = TokenSet.create(JSTokenTypes.IDENTIFIER);
    private static final ResolveCache.PolyVariantResolver<TypeScriptQualifiedNameMixin> RESOLVER =
        (reference, incompleteCode) -> reference.doResolve();

    public TypeScriptQualifiedNameMixin(@Nonnull ASTNode node) {
        super(node);
    }

    private boolean isTypeReference() {
        ASTNode parent = getNode().getTreeParent();
        return parent != null && parent.getElementType() == TypeScriptTypes.TYPE_REFERENCE;
    }

    @Override
    public PsiReference getReference() {
        return isTypeReference() ? this : null;
    }

    @Nonnull
    @Override
    public PsiReference[] getReferences() {
        return isTypeReference() ? new PsiReference[]{this} : PsiReference.EMPTY_ARRAY;
    }

    @Nonnull
    @Override
    public PsiElement getElement() {
        return this;
    }

    @Nullable
    @RequiredReadAction
    private PsiElement getLastIdentifier() {
        ASTNode[] identifiers = getNode().getChildren(IDENTIFIERS);
        return identifiers.length == 0 ? null : identifiers[identifiers.length - 1].getPsi();
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public TextRange getRangeInElement() {
        PsiElement last = getLastIdentifier();
        return last != null ? TextRange.from(last.getStartOffsetInParent(), last.getTextLength()) : TextRange.from(0, getTextLength());
    }

    @Nonnull
    @Override
    public ResolveResult[] multiResolve(boolean incompleteCode) {
        return ResolveCache.getInstance(getProject()).resolveWithCaching(this, RESOLVER, true, incompleteCode);
    }

    @RequiredReadAction
    private ResolveResult[] doResolve() {
        ASTNode[] identifiers = getNode().getChildren(IDENTIFIERS);
        if (identifiers.length == 0) {
            return ResolveResult.EMPTY_ARRAY;
        }
        List<PsiElement> current = resolveFirst(identifiers[0].getText());
        for (int i = 1; i < identifiers.length && !current.isEmpty(); i++) {
            List<PsiElement> next = new ArrayList<>();
            for (PsiElement container : current) {
                next.addAll(TypeScriptResolveUtil.findQualifiedMembers(container, identifiers[i].getText()));
            }
            current = next;
        }
        return PsiElementResolveResult.createResults(current);
    }

    @RequiredReadAction
    private List<PsiElement> resolveFirst(String name) {
        ResolveProcessor processor = new ResolveProcessor(name, this);
        JSResolveUtil.treeWalkUp(processor, this, this, this);
        List<PsiElement> result = new ArrayList<>();
        for (ResolveResult resolveResult : processor.getResultsAsResolveResults()) {
            PsiElement element = resolveResult.getElement();
            if (element instanceof TypeScriptImportBindingMixin binding) {
                List<PsiElement> imported = binding.resolveTargets();
                result.addAll(imported.isEmpty() ? List.of(binding) : imported);
            }
            else if (element != null && TypeScriptResolveUtil.isTypeDeclaration(element)) {
                result.add(element);
            }
        }
        if (result.isEmpty()) {
            PsiFile containingFile = getContainingFile();
            for (JSClass element : StubIndex.getElements(
                JavaScriptIndexKeys.CLASSES_BY_NAME, name, getProject(), JavaScriptLibraries.restrictResolveScope(this, getResolveScope()), JSClass.class)) {
                PsiFile file = element.getContainingFile();
                if (file != null && !file.equals(containingFile) && TypeScriptModuleUtil.isGlobalDeclaration(element)) {
                    result.add(element);
                }
            }
        }
        if (result.isEmpty()) {
            // the global type aliases (Record, Partial, PropertyKey of lib.es5.d.ts)
            PsiFile containingFile = getContainingFile();
            for (JSQualifiedNamedElement element : StubIndex.getElements(
                JavaScriptIndexKeys.ELEMENTS_BY_NAME, name, getProject(), JavaScriptLibraries.restrictResolveScope(this, getResolveScope()), JSQualifiedNamedElement.class)) {
                PsiFile file = element.getContainingFile();
                if (element instanceof JSTypeOnlyDeclaration && file != null && !file.equals(containingFile)
                    && TypeScriptModuleUtil.isGlobalDeclaration(element)) {
                    result.add(element);
                }
            }
        }
        return result;
    }

    @Nullable
    @Override
    public PsiElement resolve() {
        ResolveResult[] results = multiResolve(false);
        return results.length > 0 ? results[0].getElement() : null;
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public String getCanonicalText() {
        return getText();
    }

    @Override
    @RequiredReadAction
    public PsiElement handleElementRename(@Nonnull String newElementName) throws IncorrectOperationException {
        TypeScriptResolveUtil.rename(getLastIdentifier(), newElementName);
        return this;
    }

    @Override
    public PsiElement bindToElement(@Nonnull PsiElement element) throws IncorrectOperationException {
        return this;
    }

    @Override
    public boolean isReferenceTo(@Nonnull PsiElement element) {
        PsiManager manager = getManager();
        for (ResolveResult result : multiResolve(false)) {
            if (manager.areElementsEquivalent(result.getElement(), element)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public boolean isSoft() {
        return false;
    }

    // --- JSReferenceListMember (interface A extends B, class C implements D) ---

    @Override
    @RequiredReadAction
    public String getReferenceText() {
        return getText();
    }

    @Nullable
    @Override
    @RequiredReadAction
    public String getReferencedName() {
        PsiElement last = getLastIdentifier();
        return last != null ? last.getText() : null;
    }

    @Nullable
    @Override
    public JSReferenceExpression getExpression() {
        return null;
    }
}

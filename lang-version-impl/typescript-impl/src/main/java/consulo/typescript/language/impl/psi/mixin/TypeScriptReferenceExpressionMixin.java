package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.util.JSLookupUtil;
import com.intellij.lang.javascript.psi.resolve.VariantsProcessor;
import consulo.typescript.language.impl.psi.TypeScriptModuleUtil;
import consulo.language.psi.stub.StubIndex;
import consulo.javascript.language.psi.stub.JavaScriptIndexKeys;
import com.intellij.lang.javascript.psi.JSQualifiedNamedElement;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.JSThisExpression;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import com.intellij.lang.javascript.psi.resolve.WalkUpResolveProcessor;
import consulo.annotation.access.RequiredReadAction;
import consulo.document.util.TextRange;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.psi.*;
import consulo.language.psi.resolve.ResolveCache;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.typescript.language.impl.psi.TypeScriptResolveUtil;
import consulo.typescript.language.psi.TypeScriptTypes;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Mixin for TypeScript reference expressions.
 * Implements JSReferenceExpression (which extends PsiPolyVariantReference).
 *
 * @author VISTALL
 * @since 2026-03-17
 */
public class TypeScriptReferenceExpressionMixin extends TypeScriptExpressionMixin implements JSReferenceExpression {
    private static final ResolveCache.PolyVariantResolver<TypeScriptReferenceExpressionMixin> RESOLVER =
        (reference, incompleteCode) -> reference.doResolve();

    public TypeScriptReferenceExpressionMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSReferenceExpression(this);
        }
        else {
            super.accept(visitor);
        }
    }

    // --- JSReferenceExpression ---

    /**
     * {@code a} of {@code a.b} / {@code a?.b}: the expression this reference wraps.
     */
    @Nullable
    @Override
    @RequiredReadAction
    public JSExpression getQualifier() {
        PsiElement first = getFirstChild();
        return first instanceof JSExpression expression ? expression : null;
    }

    @Nullable
    @Override
    @RequiredReadAction
    public String getReferencedName() {
        PsiElement nameElement = getReferenceNameElement();
        return nameElement != null ? nameElement.getText() : null;
    }

    /**
     * The name token: the IDENTIFIER of {@code a}, the token after the dot of {@code a.b} (also a reserved word:
     * {@code p.catch}).
     */
    @Nullable
    @Override
    @RequiredReadAction
    public PsiElement getReferenceNameElement() {
        PsiElement last = getLastChild();
        while (last instanceof PsiWhiteSpace || last instanceof PsiComment) {
            last = last.getPrevSibling();
        }
        if (last == null || last instanceof JSExpression) {
            return null;
        }
        IElementType type = last.getNode().getElementType();
        return type == JSTokenTypes.DOT || type == TypeScriptTypes.QUEST_DOT ? null : last;
    }

    @Override
    public boolean shouldCheckReferences() {
        return false;
    }

    // --- PsiPolyVariantReference ---

    @Nonnull
    @Override
    public ResolveResult[] multiResolve(boolean incompleteCode) {
        return ResolveCache.getInstance(getProject()).resolveWithCaching(this, RESOLVER, true, incompleteCode);
    }

    @RequiredReadAction
    private ResolveResult[] doResolve() {
        String name = getReferencedName();
        if (name == null) {
            return ResolveResult.EMPTY_ARRAY;
        }

        JSExpression qualifier = getQualifier();
        if (qualifier == null) {
            // the scopes up to the file (JavaScript's local resolve), then the global symbols
            ResolveProcessor processor = new ResolveProcessor(name, this);
            JSResolveUtil.treeWalkUp(processor, this, this, this);
            if (processor.getResult() != null) {
                ResolveResult[] values = withoutTypeOnly(processor.getResultsAsResolveResults());
                if (values.length > 0) {
                    return followImports(values);
                }
            }

            // global declarations of the other files that aren't modules (scripts, .d.ts without import/export)
            List<PsiElement> globals = new ArrayList<>();
            PsiFile containingFile = getContainingFile();
            for (JSQualifiedNamedElement element : StubIndex.getElements(
                JavaScriptIndexKeys.ELEMENTS_BY_NAME, name, getProject(), getResolveScope(), JSQualifiedNamedElement.class)) {
                PsiFile file = element.getContainingFile();
                if (file != null && !file.equals(containingFile) && !TypeScriptModuleUtil.isModule(file)) {
                    globals.add(element);
                }
            }
            if (!globals.isEmpty()) {
                return PsiElementResolveResult.createResults(globals);
            }

            WalkUpResolveProcessor globalProcessor = new WalkUpResolveProcessor(name, null, containingFile, false, this);
            JSResolveUtil.processGlobalSymbols(this, globalProcessor);
            return globalProcessor.getResults();
        }

        // import * as ns from './x'; ns.member / Enum.MEMBER / Namespace.member
        if (qualifier instanceof JSReferenceExpression containerReference) {
            PsiElement container = containerReference.resolve();
            if (container instanceof PsiFile
                || (container != null && (container.getNode().getElementType() == TypeScriptTypes.ENUM_DECLARATION
                || container.getNode().getElementType() == TypeScriptTypes.NAMESPACE_DECLARATION))) {
                return PsiElementResolveResult.createResults(TypeScriptResolveUtil.findQualifiedMembers(container, name));
            }
        }

        JSClass jsClass;
        boolean staticAccess = false;
        if (qualifier instanceof JSThisExpression) {
            jsClass = PsiTreeUtil.getParentOfType(this, JSClass.class);
        }
        else if (qualifier.getNode().getElementType() == TypeScriptTypes.SUPER_EXPRESSION) {
            JSClass enclosing = PsiTreeUtil.getParentOfType(this, JSClass.class);
            List<JSClass> superClasses = enclosing == null ? List.of() : TypeScriptResolveUtil.getSuperClasses(enclosing);
            jsClass = superClasses.isEmpty() ? null : superClasses.get(0);
        }
        else if (qualifier instanceof JSReferenceExpression qualifierReference && qualifierReference.resolve() instanceof JSClass qualifierClass) {
            jsClass = qualifierClass;
            staticAccess = true;
        }
        else {
            jsClass = TypeScriptResolveUtil.evaluateClass(qualifier, new HashSet<>());
        }
        if (jsClass == null) {
            return ResolveResult.EMPTY_ARRAY;
        }

        List<PsiElement> members = TypeScriptResolveUtil.findMembers(jsClass, name);
        List<PsiElement> matching = new ArrayList<>();
        for (PsiElement member : members) {
            if (TypeScriptResolveUtil.isStatic(member) == staticAccess) {
                matching.add(member);
            }
        }
        return PsiElementResolveResult.createResults(matching.isEmpty() ? members : matching);
    }

    /**
     * In an expression a name is a value: interfaces, type aliases and type parameters don't count.
     */
    private static ResolveResult[] withoutTypeOnly(ResolveResult[] results) {
        List<ResolveResult> values = new ArrayList<>();
        for (ResolveResult result : results) {
            if (result.getElement() == null || !TypeScriptResolveUtil.isTypeOnly(result.getElement())) {
                values.add(result);
            }
        }
        return values.size() == results.length ? results : values.toArray(ResolveResult.EMPTY_ARRAY);
    }

    /**
     * A name declared by an import resolves to what the module exports (the binding itself when the module or the
     * export isn't found).
     */
    @RequiredReadAction
    private static ResolveResult[] followImports(ResolveResult[] results) {
        List<PsiElement> targets = new ArrayList<>();
        boolean changed = false;
        for (ResolveResult result : results) {
            if (result.getElement() instanceof TypeScriptImportBindingMixin binding) {
                List<PsiElement> imported = binding.resolveTargets();
                if (!imported.isEmpty()) {
                    targets.addAll(imported);
                    changed = true;
                    continue;
                }
            }
            targets.add(result.getElement());
        }
        return changed ? PsiElementResolveResult.createResults(targets) : results;
    }

    /**
     * Completion variants: the names in scope and the global symbols (JavaScript's VariantsProcessor), or the members
     * of the qualifier (class, enum, namespace, module).
     */
    @Nonnull
    @Override
    @RequiredReadAction
    public Object[] getVariants() {
        JSExpression qualifier = getQualifier();
        if (qualifier == null) {
            ResolveProcessor localProcessor = new ResolveProcessor(null, this);
            JSResolveUtil.treeWalkUp(localProcessor, this, this, this);
            List<Object> variants = new ArrayList<>();
            Set<String> names = new HashSet<>();
            for (PsiElement element : localProcessor.getResults()) {
                if (!TypeScriptResolveUtil.isTypeOnly(element) && element instanceof PsiNamedElement named
                    && named.getName() != null && names.add(named.getName())) {
                    variants.add(JSLookupUtil.createLookupItem(element, named.getName(), JSLookupUtil.LookupPriority.HIGHEST));
                }
            }
            // the global symbols of the JavaScript SDK
            VariantsProcessor processor = new VariantsProcessor(null, getContainingFile(), false, this);
            JSResolveUtil.processGlobalSymbols(this, processor);
            variants.addAll(List.of(processor.getResult()));
            return variants.toArray();
        }

        List<PsiElement> members = new ArrayList<>();
        PsiElement container = qualifier instanceof JSReferenceExpression reference ? reference.resolve() : null;
        if (container instanceof PsiFile module) {
            members.addAll(TypeScriptModuleUtil.findAllExports(module));
        }
        else if (container != null && (container.getNode().getElementType() == TypeScriptTypes.ENUM_DECLARATION
            || container.getNode().getElementType() == TypeScriptTypes.NAMESPACE_DECLARATION)) {
            members.addAll(TypeScriptResolveUtil.findQualifiedMembers(container, null));
        }
        else {
            JSClass jsClass = qualifier instanceof JSThisExpression ? PsiTreeUtil.getParentOfType(this, JSClass.class)
                : container instanceof JSClass containerClass ? containerClass
                : TypeScriptResolveUtil.evaluateClass(qualifier, new HashSet<>());
            if (jsClass != null) {
                members.addAll(TypeScriptResolveUtil.findAllMembers(jsClass));
            }
        }
        List<Object> variants = new ArrayList<>();
        for (PsiElement member : members) {
            if (member instanceof PsiNamedElement named && named.getName() != null) {
                variants.add(JSLookupUtil.createLookupItem(member, named.getName(), JSLookupUtil.LookupPriority.HIGHER));
            }
        }
        return variants.toArray();
    }

    // --- PsiReference ---

    @Nonnull
    @Override
    public PsiElement getElement() {
        return this;
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public TextRange getRangeInElement() {
        PsiElement nameElement = getReferenceNameElement();
        return nameElement != null ? TextRange.from(nameElement.getStartOffsetInParent(), nameElement.getTextLength())
            : TextRange.from(getTextLength(), 0);
    }

    @Nullable
    @Override
    public PsiElement resolve() {
        ResolveResult[] results = multiResolve(false);
        for (ResolveResult result : results) {
            if (result.isValidResult()) {
                return result.getElement();
            }
        }
        return null;
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public String getCanonicalText() {
        String name = getReferencedName();
        return name != null ? name : getText();
    }

    @Override
    @RequiredReadAction
    public PsiElement handleElementRename(@Nonnull String newElementName) throws IncorrectOperationException {
        TypeScriptResolveUtil.rename(getReferenceNameElement(), newElementName);
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

    @Override
    public PsiReference getReference() {
        return this;
    }
}

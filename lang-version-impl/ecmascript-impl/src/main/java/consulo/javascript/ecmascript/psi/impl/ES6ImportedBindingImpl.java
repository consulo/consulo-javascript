package consulo.javascript.ecmascript.psi.impl;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.impl.JSElementImpl;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.javascript.ecmascript.psi.ES6FromClause;
import consulo.javascript.ecmascript.psi.ES6ImportDeclaration;
import consulo.javascript.ecmascript.psi.ES6ImportSpecifier;
import consulo.javascript.ecmascript.psi.ES6ImportedBinding;
import consulo.javascript.module.resolve.JSModuleExport;
import consulo.javascript.module.resolve.JavaScriptModuleExports;
import consulo.javascript.module.resolve.JavaScriptModuleRequest;
import consulo.javascript.module.resolve.JavaScriptModuleResolution;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;

import org.jspecify.annotations.Nullable;

import java.util.List;

/**
 * @author VISTALL
 * @since 2019-12-14
 */
public class ES6ImportedBindingImpl extends JSElementImpl implements ES6ImportedBinding {
    public ES6ImportedBindingImpl(ASTNode node) {
        super(node);
    }

    @Override
    protected void accept(JSElementVisitor visitor) {
        visitor.visitJSElement(this);
    }

    @RequiredReadAction
    @Override
    public String getName() {
        PsiElement nameIdentifier = getNameIdentifier();
        return nameIdentifier == null ? null : nameIdentifier.getText();
    }

    @RequiredReadAction
    @Nullable
    @Override
    public PsiElement getNameIdentifier() {
        ES6ImportSpecifier importSpecifier = getImportSpecifier();
        if (importSpecifier != null) {
            return importSpecifier.getNameIdentifier();
        }
        return findChildByType(JSTokenTypes.IDENTIFIER);
    }

    @RequiredReadAction
    @Override
    public int getTextOffset() {
        PsiElement nameIdentifier = getNameIdentifier();
        return nameIdentifier == null ? super.getTextOffset() : nameIdentifier.getTextOffset();
    }

    @RequiredWriteAction
    @Override
    public PsiElement setName(String s) throws IncorrectOperationException {
        return null;
    }

    @Override
    public boolean processDeclarations(
        PsiScopeProcessor processor,
        ResolveState state,
        PsiElement lastParent,
        PsiElement place
    ) {
        return processor.execute(this, state);
    }

    @RequiredReadAction
    @Nullable
    @Override
    public ES6ImportSpecifier getImportSpecifier() {
        return findChildByClass(ES6ImportSpecifier.class);
    }

    /**
     * The module by the shared resolvers: its declarations (a .d.ts of the package) first, then its code
     */
    @RequiredReadAction
    @Override
    public List<PsiElement> resolveTargets() {
        String specifier = getModuleSpecifier();
        String importedName = getImportedName();
        if (specifier == null || importedName == null) {
            return List.of();
        }
        String exportedName = JSModuleExport.NAMESPACE_EXPORT.equals(importedName) ? null : importedName;

        for (JavaScriptModuleRequest.Kind kind : new JavaScriptModuleRequest.Kind[]{JavaScriptModuleRequest.Kind.TYPES, JavaScriptModuleRequest.Kind.IMPORT}) {
            PsiFile module = JavaScriptModuleResolution.resolve(this, specifier, kind);
            if (module == null) {
                continue;
            }
            if (exportedName == null) {
                return List.of(module);
            }
            List<PsiElement> exports = JavaScriptModuleExports.findExports(module, exportedName);
            if (!exports.isEmpty()) {
                return exports;
            }
        }
        return List.of();
    }

    @RequiredReadAction
    @Override
    public @Nullable String getImportedName() {
        if (findChildByType(JSTokenTypes.ASTERISK) != null) {
            return JSModuleExport.NAMESPACE_EXPORT;
        }
        if (getParent() instanceof ES6ImportDeclaration) {
            return JavaScriptModuleExports.DEFAULT_EXPORT;
        }
        // import { a as b }: the first identifier is the exported name
        PsiElement identifier = findChildByType(JSTokenTypes.IDENTIFIER);
        return identifier == null ? null : identifier.getText();
    }

    @RequiredReadAction
    @Override
    public @Nullable String getModuleSpecifier() {
        ES6ImportDeclaration declaration = PsiTreeUtil.getParentOfType(this, ES6ImportDeclaration.class);
        ES6FromClause fromClause = declaration == null ? null : declaration.getFromClause();
        String quoted = fromClause == null ? null : fromClause.getReferenceText();
        return quoted == null || quoted.length() < 2 ? null : quoted.substring(1, quoted.length() - 1);
    }
}

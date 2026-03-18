package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.stubs.JSReferenceListStub;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.impl.psi.stub.StubBasedPsiElementBase;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.stub.IStubElementType;
import consulo.language.psi.util.PsiTreeUtil;
import jakarta.annotation.Nonnull;

/**
 * Mixin for extends_list and implements_list rules.
 * JSReferenceList extends StubBasedPsiElement.
 *
 * @author VISTALL
 * @since 2026-03-18
 */
public class TypeScriptReferenceListMixin extends StubBasedPsiElementBase<JSReferenceListStub> implements JSReferenceList {
    public TypeScriptReferenceListMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public IStubElementType getElementType() {
        JSReferenceListStub stub = getStub();
        if (stub != null) {
            return stub.getStubType();
        }
        return null;
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSReferenceList(this);
        }
        else {
            visitor.visitElement(this);
        }
    }

    @RequiredReadAction
    @Override
    public JSReferenceExpression[] getExpressions() {
        JSReferenceExpression[] result = PsiTreeUtil.getChildrenOfType(this, JSReferenceExpression.class);
        return result != null ? result : new JSReferenceExpression[0];
    }

    @RequiredReadAction
    @Override
    public String[] getReferenceTexts() {
        JSReferenceExpression[] exprs = getExpressions();
        String[] texts = new String[exprs.length];
        for (int i = 0; i < exprs.length; i++) {
            texts[i] = exprs[i].getText();
        }
        return texts;
    }

    @Override
    public JSClass[] getReferencedClasses() {
        return new JSClass[0];
    }

    @Override
    public String toString() {
        return getClass().getSimpleName() + ":" + getNode().getElementType();
    }
}

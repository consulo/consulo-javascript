package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSParameterList;
import com.intellij.lang.javascript.psi.stubs.JSParameterListStub;
import consulo.language.ast.ASTNode;
import consulo.language.impl.psi.stub.StubBasedPsiElementBase;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.stub.IStubElementType;
import consulo.language.psi.util.PsiTreeUtil;
import jakarta.annotation.Nonnull;

/**
 * Mixin for TypeScript parameter lists.
 * Implements JSParameterList to integrate with JavaScript PSI infrastructure.
 *
 * @author VISTALL
 * @since 2026-03-18
 */
public class TypeScriptParameterListMixin extends StubBasedPsiElementBase<JSParameterListStub> implements JSParameterList {
    public TypeScriptParameterListMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public IStubElementType getElementType() {
        JSParameterListStub stub = getStub();
        if (stub != null) {
            return stub.getStubType();
        }
        return null;
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSParameterList(this);
        }
        else {
            visitor.visitElement(this);
        }
    }

    // --- JSParameterList ---

    @Override
    public JSParameter[] getParameters() {
        JSParameter[] params = PsiTreeUtil.getChildrenOfType(this, JSParameter.class);
        return params != null ? params : JSParameter.EMPTY_ARRAY;
    }
}

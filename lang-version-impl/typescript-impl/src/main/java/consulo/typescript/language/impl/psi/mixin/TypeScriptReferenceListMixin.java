package consulo.typescript.language.impl.psi.mixin;

import java.util.List;
import java.util.ArrayList;
import consulo.typescript.language.psi.TypeScriptTypes;
import com.intellij.lang.javascript.psi.ecmal4.JSReferenceListMember;
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

    /**
     * The extends list of an interface (stubbed).
     */
    public TypeScriptReferenceListMixin(@Nonnull JSReferenceListStub stub, @Nonnull IStubElementType type) {
        super(stub, type);
    }

    @Override
    public IStubElementType getElementType() {
        JSReferenceListStub stub = getStub();
        if (stub != null) {
            return stub.getStubType();
        }
        return getNode().getElementType() instanceof IStubElementType stubType ? stubType : null;
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
        // the stub of an interface's list: the type names, the inherited members resolve without the AST
        JSReferenceListStub stub = getGreenStub();
        if (stub != null) {
            return stub.getReferenceTexts();
        }
        if (getNode().getElementType() == TypeScriptTypes.EXTENDS_LIST) {
            List<String> names = new ArrayList<>();
            for (JSReferenceListMember member : getMembers()) {
                names.add(member.getReferenceText());
            }
            return names.toArray(String[]::new);
        }
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

    /**
     * The references of the list: expressions ({@code class A extends B}) and type names ({@code interface A extends B}).
     */
    @RequiredReadAction
    @Override
    public JSReferenceListMember[] getMembers() {
        List<JSReferenceListMember> members = new ArrayList<>();
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSReferenceListMember member) {
                members.add(member);
            }
            else if (child.getNode().getElementType() == TypeScriptTypes.TYPE_REFERENCE) {
                ASTNode name = child.getNode().findChildByType(TypeScriptTypes.QUALIFIED_NAME);
                if (name != null && name.getPsi() instanceof JSReferenceListMember member) {
                    members.add(member);
                }
            }
        }
        return members.toArray(JSReferenceListMember.EMPTY_ARRAY);
    }

    @Override
    public String toString() {
        return getClass().getSimpleName() + ":" + getNode().getElementType();
    }
}

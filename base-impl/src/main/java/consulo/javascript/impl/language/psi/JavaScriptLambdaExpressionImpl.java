/*
 * Copyright 2013-2016 must-be.org
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package consulo.javascript.impl.language.psi;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSExpressionImpl;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.javascript.language.psi.JavaScriptType;
import consulo.javascript.language.psi.JavaScriptTypeElement;
import consulo.javascript.psi.JavaScriptLambdaExpression;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.util.IncorrectOperationException;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import org.jetbrains.annotations.NonNls;

/**
 * @author VISTALL
 * @since 03.03.2016
 */
public class JavaScriptLambdaExpressionImpl extends JSExpressionImpl implements JavaScriptLambdaExpression {
    public JavaScriptLambdaExpressionImpl(ASTNode node) {
        super(node);
    }

    @Override
    @RequiredReadAction
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState state,
        PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        if (lastParent != null && lastParent.getParent() == this) {
            final JSParameter[] params = getParameterList().getParameters();
            for (JSParameter param : params) {
                if (!processor.execute(param, state)) {
                    return false;
                }
            }

            boolean b = JSResolveUtil.processDeclarationsInScope(this, processor, state, lastParent, place);
            if (b) {
                processor.handleEvent(PsiScopeProcessor.Event.SET_DECLARATION_HOLDER, this);
            }
            return b;
        }

        return processor.execute(this, state);
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitLambdaExpression(this);
    }

    @RequiredReadAction
    @Nonnull
    @Override
    public JSParameterList getParameterList() {
        return findNotNullChildByClass(JSParameterList.class);
    }

    @Override
    public JSSourceElement[] getBody() {
        final ASTNode[] children = getNode().getChildren(JSElementTypes.SOURCE_ELEMENTS);
        if (children.length == 0) {
            return JSSourceElement.EMPTY_ARRAY;
        }
        JSSourceElement[] result = new JSSourceElement[children.length];
        for (int i = 0; i < children.length; i++) {
            result[i] = (JSSourceElement)children[i].getPsi();
        }
        return result;
    }

    @Nonnull
    @Override
    public JavaScriptType getReturnType() {
        return JavaScriptType.UNKNOWN;
    }

    @Override
    public String getReturnTypeString() {
        return null;
    }

    @Nullable
    @Override
    public JavaScriptTypeElement getReturnTypeElement() {
        return null;
    }

    @Override
    public boolean isDeprecated() {
        return false;
    }

    @Nullable
    @Override
    public JSAttributeList getAttributeList() {
        return null;
    }

    @Override
    public String getQualifiedName() {
        return null;
    }

    @RequiredReadAction
    @Nullable
    @Override
    public PsiElement getNameIdentifier() {
        return null;
    }

    @RequiredWriteAction
    @Override
    public PsiElement setName(@Nonnull @NonNls String name) throws IncorrectOperationException {
        return null;
    }
}

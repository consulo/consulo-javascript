package com.intellij.lang.javascript.psi.impl;

import com.intellij.lang.javascript.JSDocTokenTypes;
import com.intellij.lang.javascript.psi.JSDocComment;
import com.intellij.lang.javascript.psi.JSDocTag;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.ast.TokenSet;
import consulo.language.impl.psi.LazyParseablePsiElement;
import consulo.language.psi.PsiElementVisitor;
import org.jspecify.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

/**
 * A {@code /** ... *}{@code /} comment: parsed lazily ({@link com.intellij.lang.javascript.JSTokenTypes#DOC_COMMENT}),
 * and a {@link consulo.language.psi.PsiComment} - the documentation and the JSDoc types of declarations
 * ({@code @param {HTMLElement} element}) find it so.
 *
 * @since 2026-10-31
 */
public class JSDocCommentElement extends LazyParseablePsiElement implements JSDocComment {
    private static final TokenSet DOC_TAGS = TokenSet.create(JSDocTokenTypes.DOC_TAG);

    public JSDocCommentElement(IElementType type, @Nullable CharSequence buffer) {
        super(type, buffer);
    }

    @Override
    public IElementType getTokenType() {
        return getElementType();
    }

    @Override
    @RequiredReadAction
    public JSDocTag[] getTags() {
        List<JSDocTag> tags = new ArrayList<>();
        for (ASTNode tag : getChildren(DOC_TAGS)) {
            if (tag.getPsi() instanceof JSDocTag docTag) {
                tags.add(docTag);
            }
        }
        return tags.toArray(new JSDocTag[0]);
    }

    @Override
    public void accept(PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSDocComment(this);
        }
        else {
            super.accept(visitor);
        }
    }
}

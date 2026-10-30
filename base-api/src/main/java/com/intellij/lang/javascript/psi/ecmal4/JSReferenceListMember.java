package com.intellij.lang.javascript.psi.ecmal4;

import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import consulo.annotation.access.RequiredReadAction;
import org.jspecify.annotations.Nullable;

/**
 * A member of a reference list ({@code extends A, B} / {@code implements C}): IntelliJ's API. In Consulo the members
 * are the references themselves (JavaScript reference expressions, TypeScript type names).
 *
 * @since 2026-10-31
 */
public interface JSReferenceListMember extends JSElement {
    JSReferenceListMember[] EMPTY_ARRAY = new JSReferenceListMember[0];

    /**
     * The text of the reference ({@code ns.Base}).
     */
    @Nullable
    @RequiredReadAction
    String getReferenceText();

    /**
     * The last name of the reference ({@code Base} of {@code ns.Base}).
     */
    @Nullable
    @RequiredReadAction
    String getReferencedName();

    /**
     * The reference expression, when the member is one.
     */
    @Nullable
    @RequiredReadAction
    JSReferenceExpression getExpression();
}

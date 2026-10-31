package consulo.javascript.ecmascript.psi;

import com.intellij.lang.javascript.psi.JSElement;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiNameIdentifierOwner;
import org.jspecify.annotations.Nullable;

import java.util.List;

/**
 * @author VISTALL
 * @since 2019-12-14
 * <p>
 * import ImportedBinding ....
 * <p>
 * asterisk import also imported binding
 * <p>
 * import * as util ...
 */
public interface ES6ImportedBinding extends JSElement, PsiNameIdentifierOwner {
    @Nullable
    @RequiredReadAction
    ES6ImportSpecifier getImportSpecifier();

    /**
     * The declarations the binding imports: the export of its name ({@code default} for a default import), the module
     * itself for {@code * as ns}; empty when the module or the export isn't found.
     */
    @RequiredReadAction
    List<PsiElement> resolveTargets();

    /**
     * @return the name the module exports: {@code a} of {@code { a as b }}, {@code default} of a default import,
     * {@code *} of {@code * as ns}
     */
    @RequiredReadAction
    @Nullable
    String getImportedName();

    /**
     * @return the module, unquoted
     */
    @RequiredReadAction
    @Nullable
    String getModuleSpecifier();
}

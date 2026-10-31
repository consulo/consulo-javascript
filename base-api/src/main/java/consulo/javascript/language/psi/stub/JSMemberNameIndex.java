package consulo.javascript.language.psi.stub;

import com.intellij.lang.javascript.psi.JSQualifiedNamedElement;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.stub.StringStubIndexExtension;
import consulo.language.psi.stub.StubIndexKey;

/**
 * {@link JavaScriptIndexKeys#MEMBERS_BY_NAME}.
 *
 * @since 2026-10-31
 */
@ExtensionImpl
public class JSMemberNameIndex extends StringStubIndexExtension<JSQualifiedNamedElement> {
    @Override
    public StubIndexKey<String, JSQualifiedNamedElement> getKey() {
        return JavaScriptIndexKeys.MEMBERS_BY_NAME;
    }
}

package consulo.javascript.debugger.cdt;

import com.github.kklisura.cdt.protocol.types.debugger.Scope;
import com.github.kklisura.cdt.protocol.types.debugger.ScopeType;
import com.github.kklisura.cdt.protocol.types.runtime.RemoteObject;
import consulo.execution.debug.frame.XCompositeNode;
import consulo.execution.debug.frame.XValueGroup;
import consulo.util.lang.StringUtil;
import jakarta.annotation.Nonnull;

import java.util.Locale;

/**
 * @author VISTALL
 * @since 2026-02-15
 */
public class CDTScopeValue extends XValueGroup {
    @Nonnull
    private final Scope myScope;
    @Nonnull
    private final RemoteObject myRemoteObject;
    private final CDTProcessBase myProcess;

    public CDTScopeValue(@Nonnull Scope scope,
                         @Nonnull RemoteObject remoteObject,
                         CDTProcessBase process) {
        super(StringUtil.capitalize(scope.getType().name().toLowerCase(Locale.ROOT)));
        myScope = scope;
        myRemoteObject = remoteObject;
        myProcess = process;
    }

    @Override
    public void computeChildren(@Nonnull XCompositeNode node) {
        CDTRemoteObjectValue.fill(myRemoteObject, node, myProcess);
    }

    @Override
    public boolean isAutoExpand() {
        return myScope.getType() == ScopeType.LOCAL;
    }
}

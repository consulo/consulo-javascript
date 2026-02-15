package consulo.javascript.debugger.cdt;

import com.github.kklisura.cdt.protocol.types.debugger.Scope;
import com.github.kklisura.cdt.protocol.types.runtime.RemoteObject;
import consulo.platform.base.icon.PlatformIconGroup;
import consulo.ui.image.Image;
import consulo.util.lang.StringUtil;
import jakarta.annotation.Nonnull;

import java.util.Locale;

/**
 * @author VISTALL
 * @since 2026-02-15
 */
public class CDTScopeValue extends CDTRemoteObjectValue {
    public CDTScopeValue(@Nonnull Scope scope, @Nonnull RemoteObject remoteObject, CDTProcessBase process) {
        super(StringUtil.capitalize(scope.getType().name().toLowerCase(Locale.ROOT)), remoteObject, process);
    }

    @Nonnull
    @Override
    protected Image getObjectIcon() {
        return PlatformIconGroup.nodesFolder();
    }
}

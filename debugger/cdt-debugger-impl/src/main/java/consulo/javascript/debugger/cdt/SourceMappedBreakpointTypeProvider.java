package consulo.javascript.debugger.cdt;

import consulo.annotation.component.ComponentScope;
import consulo.annotation.component.ExtensionAPI;
import consulo.execution.debug.breakpoint.XLineBreakpointType;

/**
 * A line breakpoint type of a language compiled to JavaScript: its breakpoints bind
 * through the source maps of the scripts a Chrome DevTools session loads (browser and
 * node sessions alike).
 *
 * @author VISTALL
 * @since 2026-11-01
 */
@ExtensionAPI(ComponentScope.APPLICATION)
public interface SourceMappedBreakpointTypeProvider {
    Class<? extends XLineBreakpointType<?>> getBreakpointType();
}

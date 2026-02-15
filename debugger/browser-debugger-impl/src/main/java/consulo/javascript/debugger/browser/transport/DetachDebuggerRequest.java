package consulo.javascript.debugger.browser.transport;

/**
 * @author VISTALL
 * @since 2026-02-15
 */
public class DetachDebuggerRequest {
    public String message = "detach_debugger";
    public final int tabId;

    public DetachDebuggerRequest(int tabId) {
        this.tabId = tabId;
    }
}

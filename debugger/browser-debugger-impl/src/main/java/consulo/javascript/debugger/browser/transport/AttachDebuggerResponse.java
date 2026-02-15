package consulo.javascript.debugger.browser.transport;

/**
 * @author VISTALL
 * @since 2026-02-15
 */
public class AttachDebuggerResponse {
    public String message = "attach_debugger";
    public int tabId;

    public AttachDebuggerResponse(int tabId) {
        this.tabId = tabId;
    }
}

package consulo.javascript.debugger.browser;

import consulo.builtinWebServer.webSocket.WebSocketConnection;
import consulo.execution.debug.DefaultDebugProcessHandler;
import consulo.javascript.debugger.browser.process.CDTProcess;
import consulo.javascript.debugger.browser.transport.DetachDebuggerRequest;
import consulo.javascript.debugger.browser.transport.ExtensionWebSocketAccepter;
import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2026-02-15
 */
public class BrowserDebuggerProcess extends DefaultDebugProcessHandler {
    private final SessionHolder.BrowserSession myBrowserSession;

    public BrowserDebuggerProcess(@Nonnull SessionHolder.BrowserSession browserSession) {
        myBrowserSession = browserSession;
    }

    public void setDebugProcess(CDTProcess cdtProcess) {
        myBrowserSession.setDebugProcess(cdtProcess);
    }

    @Override
    protected void destroyProcessImpl() {
        super.destroyProcessImpl();

        WebSocketConnection connection = myBrowserSession.getWebSocketConnection();
        if (connection != null) {
            connection.send(ExtensionWebSocketAccepter.GSON.toJson(new DetachDebuggerRequest(myBrowserSession.getTabId())));
        }

        myBrowserSession.close();
    }
}

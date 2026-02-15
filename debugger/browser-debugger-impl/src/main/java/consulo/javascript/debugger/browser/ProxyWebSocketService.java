package consulo.javascript.debugger.browser;

import com.github.kklisura.cdt.services.WebSocketService;
import com.github.kklisura.cdt.services.exceptions.WebSocketServiceException;
import consulo.builtinWebServer.webSocket.WebSocketConnection;
import consulo.javascript.debugger.browser.transport.DebuggerCommandRequest;
import consulo.javascript.debugger.browser.transport.ExtensionWebSocketAccepter;

import java.net.URI;
import java.util.function.Consumer;

/**
 * @author VISTALL
 * @since 2026-02-15
 */
public class ProxyWebSocketService implements WebSocketService {
    private final SessionHolder.BrowserSession myBrowserSession;

    public ProxyWebSocketService(SessionHolder.BrowserSession browserSession) {
        myBrowserSession = browserSession;
    }

    @Override
    public void connect(URI uri) throws WebSocketServiceException {
        throw new UnsupportedOperationException();
    }

    @Override
    public void send(String message) throws WebSocketServiceException {
        WebSocketConnection connection = myBrowserSession.getWebSocketConnection();

        DebuggerCommandRequest request = new DebuggerCommandRequest();
        request.tabId = myBrowserSession.getTabId();
        request.command = ExtensionWebSocketAccepter.GSON.fromJson(message, Object.class);

        String json = ExtensionWebSocketAccepter.GSON.toJson(request);

        connection.send(json);
    }

    @Override
    public void addMessageHandler(Consumer<String> consumer) throws WebSocketServiceException {
        myBrowserSession.addHandler(consumer);
    }

    @Override
    public void close() {
    }

    @Override
    public boolean closed() {
        return false;
    }
}

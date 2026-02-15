package consulo.javascript.debugger.browser;

import consulo.annotation.component.ComponentScope;
import consulo.annotation.component.ServiceAPI;
import consulo.annotation.component.ServiceImpl;
import consulo.builtinWebServer.webSocket.WebSocketConnection;
import consulo.javascript.debugger.browser.process.CDTProcess;
import consulo.util.collection.Lists;
import consulo.util.collection.SmartList;
import jakarta.inject.Singleton;

import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;

/**
 * @author VISTALL
 * @since 2026-02-15
 */
@Singleton
@ServiceAPI(ComponentScope.APPLICATION)
@ServiceImpl
public class SessionHolder {
    public class BrowserSession {
        private final String myUrl;

        private int myTabId;

        private WebSocketConnection myWebSocketConnection;

        private CDTProcess myDebugProcess;

        private List<Consumer<String>> myHandlers = new SmartList<>();

        public BrowserSession(String url) {
            myUrl = url;
        }

        public int getTabId() {
            return myTabId;
        }

        public void addHandler(Consumer<String> consumer) {
            myHandlers.add(consumer);
        }

        public void fireHandlers(String message) {
            for (Consumer<String> handler : myHandlers) {
                handler.accept(message);
            }
        }

        public void close() {
            myBrowserSessions.remove(this);
        }

        public void setDebugProcess(CDTProcess debugProcess) {
            myDebugProcess = debugProcess;
        }

        public WebSocketConnection getWebSocketConnection() {
            return myWebSocketConnection;
        }

        public CDTProcess getDebugProcess() {
            return myDebugProcess;
        }
    }

    private List<BrowserSession> myBrowserSessions = Lists.newLockFreeCopyOnWriteList();

    public BrowserSession prepareSession(String url) {
        BrowserSession session = new BrowserSession(url);
        myBrowserSessions.add(session);
        return session;
    }

    public BrowserSession find(int tabId) {
        for (BrowserSession browserSession : myBrowserSessions) {
            if (browserSession.myTabId == tabId) {
                return browserSession;
            }
        }

        return null;
    }

    public BrowserSession initNew(String url, int tabId, WebSocketConnection connection) {
        for (BrowserSession session : myBrowserSessions.reversed()) {
            if (session.myTabId != 0) {
                continue;
            }

            if (Objects.equals(session.myUrl, url)) {
                session.myTabId = tabId;
                session. myWebSocketConnection = connection;
                return session;
            }
        }

        return null;
    }
}

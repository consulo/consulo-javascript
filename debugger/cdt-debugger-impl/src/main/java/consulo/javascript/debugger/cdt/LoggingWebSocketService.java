package consulo.javascript.debugger.cdt;

import com.github.kklisura.cdt.services.WebSocketService;
import com.github.kklisura.cdt.services.exceptions.WebSocketServiceException;

import java.net.URI;
import java.util.function.Consumer;

/**
 * @author VISTALL
 * @since 2026-02-16
 */
public class LoggingWebSocketService implements WebSocketService {
    private final WebSocketService myDelegate;

    public LoggingWebSocketService(WebSocketService delegate) {
        myDelegate = delegate;
    }

    @Override
    public void connect(URI uri) throws WebSocketServiceException {
        myDelegate.connect(uri);
    }

    @Override
    public void send(String s) throws WebSocketServiceException {
        System.out.println("send " + s);
        myDelegate.send(s);
    }

    @Override
    public void addMessageHandler(Consumer<String> consumer) throws WebSocketServiceException {
        myDelegate.addMessageHandler(s -> {
            System.out.println("rec: " + s);
            consumer.accept(s);
        });
    }

    @Override
    public void close() {
        myDelegate.close();
    }

    @Override
    public boolean closed() {
        return myDelegate.closed();
    }
}

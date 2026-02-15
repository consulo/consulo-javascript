package consulo.javascript.debugger.cdt;

import com.github.kklisura.cdt.protocol.events.debugger.ScriptParsed;
import consulo.virtualFileSystem.VirtualFile;
import consulo.virtualFileSystem.VirtualFileManager;

/**
 * @author VISTALL
 * @since 2026-02-15
 */
public class CDTScript {
    private final String myId;
    private final String myUrl;

    public CDTScript(ScriptParsed scriptParsed) {
        myId = scriptParsed.getScriptId();
        myUrl = scriptParsed.getUrl();
    }

    public String getId() {
        return myId;
    }

    public VirtualFile toVirtualFile() {
        return VirtualFileManager.getInstance().findFileByUrl(myUrl);
    }
}

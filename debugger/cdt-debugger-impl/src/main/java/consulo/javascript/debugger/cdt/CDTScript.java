package consulo.javascript.debugger.cdt;

import consulo.javascript.debugger.cdt.sourcemap.SourceMapLoader;
import consulo.javascript.debugger.cdt.sourcemap.SourceMap;
import com.github.kklisura.cdt.protocol.events.debugger.ScriptParsed;
import com.github.kklisura.cdt.protocol.types.debugger.ScriptLanguage;
import consulo.javascript.debugger.JavaScriptFileInfo;
import consulo.javascript.icon.JavaScriptIconGroup;
import consulo.language.editor.FileColorManager;
import consulo.navigation.OpenFileDescriptorFactory;
import consulo.project.Project;
import consulo.ui.color.ColorValue;
import consulo.ui.image.Image;
import consulo.virtualFileSystem.VirtualFile;
import consulo.virtualFileSystem.VirtualFileManager;
import consulo.virtualFileSystem.util.VirtualFileUtil;
import org.jspecify.annotations.Nullable;

/**
 * @author VISTALL
 * @since 2026-02-15
 */
public class CDTScript implements JavaScriptFileInfo {
    private final String myId;
    private final String myUrl;
    private final Project myProject;
    private final ScriptLanguage myScriptLanguage;
    private final @Nullable String mySourceMapUrl;

    private volatile @Nullable SourceMap mySourceMap;
    private volatile boolean mySourceMapLoaded;

    public CDTScript(Project project, ScriptParsed scriptParsed) {
        myProject = project;
        myId = scriptParsed.getScriptId();
        myUrl = scriptParsed.getUrl();
        myScriptLanguage = scriptParsed.getScriptLanguage();
        mySourceMapUrl = scriptParsed.getSourceMapURL();
    }

    public String getId() {
        return myId;
    }

    public String getUrl() {
        return myUrl;
    }

    /** The script's source map, loaded on first use; null when it declares none or it cannot be read. */
    public @Nullable SourceMap getSourceMap() {
        if (!mySourceMapLoaded) {
            synchronized (this) {
                if (!mySourceMapLoaded) {
                    mySourceMap = SourceMapLoader.load(myUrl, mySourceMapUrl);
                    mySourceMapLoaded = true;
                }
            }
        }
        return mySourceMap;
    }

    public boolean hasSourceMap() {
        return mySourceMapUrl != null && !mySourceMapUrl.isBlank();
    }

    public VirtualFile toVirtualFile() {
        return VirtualFileManager.getInstance().findFileByUrl(myUrl);
    }

    @Override
    public String getPath() {
        return/* myId + ":" +*/ VirtualFileUtil.urlToPath(myUrl);
    }

    @Override
    public Image getIcon() {
        return switch (myScriptLanguage) {
            case JAVA_SCRIPT -> JavaScriptIconGroup.javascript();
            case WEB_ASSEMBLY ->
                // TODO web assembly icon
                JavaScriptIconGroup.javascript();
        };
    }

    @Nullable
    @Override
    public ColorValue getFileStatusColor() {
        VirtualFile virtualFile = toVirtualFile();
        if (virtualFile != null) {
            return FileColorManager.getInstance(myProject).getFileColorValue(virtualFile);
        }
        return null;
    }

    @Override
    public void navigate(boolean focus) {
        VirtualFile file = toVirtualFile();
        if (file != null) {
            OpenFileDescriptorFactory.getInstance(myProject).newBuilder(file).build().navigate(focus);
        }
    }
}

package consulo.javascript.debugger;

import consulo.navigation.Navigatable;
import consulo.ui.color.ColorValue;
import consulo.ui.image.Image;
import org.jspecify.annotations.Nullable;

/**
 * @author VISTALL
 * @since 2026-02-16
 */
public interface JavaScriptFileInfo extends Navigatable {
    String getPath();

    Image getIcon();

    @Nullable
    ColorValue getFileStatusColor();
}

package consulo.javascript.debugger;

import consulo.navigation.Navigatable;
import consulo.ui.color.ColorValue;
import consulo.ui.image.Image;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * @author VISTALL
 * @since 2026-02-16
 */
public interface JavaScriptFileInfo extends Navigatable {
    @Nonnull
    String getPath();

    @Nonnull
    Image getIcon();

    @Nullable
    ColorValue getFileStatusColor();
}

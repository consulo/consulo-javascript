package consulo.javascript.language;

/**
 * Marker for javascript version which can be selected inside combobox for module
 *
 * @author VISTALL
 * @since 2023-08-31
 */
public interface StandardJavaScriptVersion {
    @Deprecated
    default int getWeight() {
        return 0;
    }
}

package consulo.javascript.language;

/**
 * Marker for javascript version which can be selected inside combobox for module
 *
 * @author VISTALL
 * @since 31/08/2023
 */
public interface StandardJavaScriptVersion {
    @Deprecated
    default int getWeight() {
        return 0;
    }
}

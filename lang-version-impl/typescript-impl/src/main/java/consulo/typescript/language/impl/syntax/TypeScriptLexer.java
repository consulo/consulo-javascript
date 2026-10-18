package consulo.typescript.language.impl.syntax;

/**
 * @author VISTALL
 * @since 2026-10-18
 */
public class TypeScriptLexer extends _TypeScriptLexer {
    private static final int BASE_STATE_MASK = 0xF;
    private static final int TEMPLATE_DEPTH_SHIFT = 4;

    @Override
    public void start(CharSequence buffer, int startOffset, int endOffset, int initialState) {
        super.start(buffer, startOffset, endOffset, initialState & BASE_STATE_MASK);
        setTemplateDepth(initialState >>> TEMPLATE_DEPTH_SHIFT);
    }

    @Override
    public int getState() {
        return super.getState() | (getTemplateDepth() << TEMPLATE_DEPTH_SHIFT);
    }
}

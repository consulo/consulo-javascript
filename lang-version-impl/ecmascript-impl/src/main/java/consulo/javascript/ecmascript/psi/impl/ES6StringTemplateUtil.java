package consulo.javascript.ecmascript.psi.impl;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.document.util.TextRange;
import consulo.javascript.ecmascript.psi.ES6StringTemplateExpression;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiFileFactory;
import consulo.language.psi.PsiLanguageInjectionHost;
import consulo.language.psi.util.PsiTreeUtil;

import java.util.ArrayList;
import java.util.List;

/**
 * @author VISTALL
 * @since 2026-10-18
 */
public final class ES6StringTemplateUtil {
    private ES6StringTemplateUtil() {
    }

    @RequiredReadAction
    public static JSExpression[] getArguments(PsiElement template) {
        List<JSExpression> arguments = new ArrayList<>();
        for (PsiElement child = template.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSExpression expression) {
                arguments.add(expression);
            }
        }
        return arguments.toArray(JSExpression.EMPTY_ARRAY);
    }

    @RequiredReadAction
    public static TextRange[] getStringRanges(PsiElement template) {
        List<TextRange> ranges = new ArrayList<>();
        for (TextRange range : getStringRangesWithEmpty(template)) {
            if (!range.isEmpty()) {
                ranges.add(range);
            }
        }
        return ranges.toArray(TextRange[]::new);
    }

    @RequiredReadAction
    public static TextRange[] getStringRangesWithEmpty(PsiElement template) {
        List<TextRange> ranges = new ArrayList<>();
        ASTNode node = template.getNode();
        int baseOffset = node.getStartOffset();
        boolean opened = false;
        boolean closed = false;
        int textStart = -1;

        for (ASTNode child = node.getFirstChildNode(); child != null; child = child.getTreeNext()) {
            IElementType type = child.getElementType();
            int start = child.getStartOffset() - baseOffset;
            int end = start + child.getTextLength();

            if (type == JSTokenTypes.BACKQUOTE) {
                if (!opened) {
                    opened = true;
                    textStart = end;
                }
                else if (!closed) {
                    if (textStart >= 0) {
                        ranges.add(new TextRange(textStart, start));
                    }
                    textStart = -1;
                    closed = true;
                }
            }
            else if (type == JSTokenTypes.DOLLAR) {
                if (textStart >= 0) {
                    ranges.add(new TextRange(textStart, start));
                }
                textStart = -1;
            }
            else if (type == JSTokenTypes.RBRACE && opened && !closed) {
                textStart = end;
            }
        }

        if (textStart >= 0) {
            ranges.add(new TextRange(textStart, node.getTextLength()));
        }
        return ranges.toArray(TextRange[]::new);
    }

    @RequiredWriteAction
    public static PsiLanguageInjectionHost updateText(ES6StringTemplateExpression template, String text) {
        PsiFile containingFile = template.getContainingFile();
        PsiFile file = PsiFileFactory.getInstance(template.getProject())
            .createFileFromText(containingFile.getName(), containingFile.getLanguage(), template.getLanguageVersion(), text);
        ES6StringTemplateExpression newTemplate = PsiTreeUtil.findChildOfType(file, ES6StringTemplateExpression.class);
        if (newTemplate == null) {
            return template;
        }
        return (PsiLanguageInjectionHost)template.replace(newTemplate);
    }
}
